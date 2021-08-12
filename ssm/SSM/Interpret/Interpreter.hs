module SSM.Interpret.Interpreter
  ( interpret
  , InterpretConfig(..)
  , interpret_
  , SSMProgram(..)
  ) where

import           SSM.Core.Syntax
import           SSM.Interpret.Internal
import qualified SSM.Interpret.Trace           as T
import           SSM.Util.Default               ( Default(def) )
import           SSM.Util.HughesList     hiding ( (++) )
import           SSM.Util.Operators             ( (<#>) )

import qualified Data.IntMap                   as IntMap
import qualified Data.Map                      as Map

import           Control.Monad
import           Control.Monad.ST.Lazy
import           Control.Monad.State.Lazy
import           Control.Monad.Writer.Lazy

-- | Interpret an SSM program with the default configuration.
interpret_ :: SSMProgram p => p -> T.Trace
interpret_ = interpret def

{-| Interpret an SSM program.

The output is the debug information specified in "SSM.Interpret.Trace", which is
used by the testing machinery to evaluate the semantics of the interpreter.

The interpreter itself is lazy, so it can handle non terminating programs without
issue. What you do to get the output in that case is to ask it for a finite amount
of output, such as @take 10000 (interpret program)@. After evaluating enough to
give you @10000@ trace items, it will not evaluate more.
-}
interpret :: SSMProgram p => InterpretConfig -> p -> T.Trace
interpret config program = runST $ do
  let p = toProgram program
  -- Fetch procedure body
  fun <- case Map.lookup (entry p) (funs p) of
    Just p' -> return p'
    Nothing ->
      error $ "Interpreter error: cannot find entry point: " ++ identName (entry p)
  globs       <- globals p
  -- Run the interpret action and produce it's output
  (_, events) <- runWriterT $ runStateT run $ initState config p 0 globs $ mkProc
    config
    p
    fun
  return $ fromHughes events

-- | Run the interpreter, serving the role of the @main@ function.
run :: Interp s ()
run = tick >> runInstant
 where
  -- | Advance model time to the next event time, and calls 'tick'.
  runInstant :: Interp s ()
  runInstant = do
    b <- eventQueueEmpty
    if b
      then tellEvent T.TerminatedOk
      else do
        es <- fromIntegral <$> eventQueueSize
        n  <- nextEventTime
        tellEvent $ T.DriverEventQueueStatus es n
        setNow n
        tick >> runInstant

  -- | Apply all scheduled updates for current instant, then 'runProcs'.
  tick :: Interp s ()
  tick = do
    performEvents
    nc <- actQueueSize
    runProcs

  -- | Pop and run processes from the ready queue until the queue is empty.
  runProcs :: Interp s ()
  runProcs = do
    acts <- actQueueSize
    unless (acts == 0) $ do
      p <- dequeue
      setCurrentProcess p
      tellEvent $ T.ActStepBegin $ procName p
      traceVars
      microtick
      step
      runProcs

{- | Run instructions of a process for the current instant.

Yields control when the process terminates (no more instructions) or suspends
(after executing a 'Fork' or 'Wait').
-}
step :: Interp s ()
step = do
  i <- nextInstruction
  p <- currentProcess
  case i of
    Nothing  -> leave

    Just stm -> case stm of
      CreateRef n t -> do
        createRef n t
        continue

      SetRef r e -> do
        writeRef r e
        continue

      SetLocal n _ e2 -> do
        writeLocal n e2
        continue

      If c thn els -> do
        b <- getBool <$> eval c
        pushInstructions $ if b then thn else els
        continue

      While c bdy -> do
        b <- getBool <$> eval c
        when b microtick
        when b $ pushInstructions $ bdy ++ [stm]
        continue

      Skip        -> continue

      After d r v -> do
        let d' = genTimeDelay d
        v' <- eval v
        n' <- getNow
        scheduleEvent r (n' + d') v'
        continue

      Yield -> yield

      Sensitize ref -> do
        tellEvent $ T.ActSensitize $ refName ref
        sensitize ref
        continue
      Desensitize ref -> do
        desensitize ref
        continue

--      Wait refs -> do
--        forM_ refs $ \r -> tellEvent $ T.ActSensitize $ refName r
--        wait refs
--        yield

      Fork procs -> do
        setRunningChildren (length procs)
        parent <- addressToSelf
        pdeps  <- pds (length procs)
        forM_ procs $ \(f, _) -> tellEvent $ T.ActActivate $ identName f
        forM_ (zip procs pdeps) $ \(f, (prio, dep)) -> fork f prio dep parent
        continue

 where
  -- | Convenience names for local control flow.
  continue, yield :: Interp s ()
  continue = step
  yield    = return ()
