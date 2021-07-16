module SSM.Interpret.Interpreter
  ( interpret
  , InterpretConfig(..)
  ) where

import           SSM.Core.Syntax
import           SSM.Interpret.Internal
import qualified SSM.Interpret.Trace           as T
import           SSM.Util.HughesList     hiding ( (++) )
import           SSM.Util.Operators             ( (<#>) )

import qualified Data.IntMap                   as IntMap
import qualified Data.Map                      as Map

import           Control.Monad
import           Control.Monad.ST.Lazy
import           Control.Monad.State.Lazy
import           Control.Monad.Writer.Lazy

{-| Interpret an SSM program.

The output is the debug information specified in "SSM.Interpret.Trace", which is
used by the testing machinery to evaluate the semantics of the interpreter.

The interpreter itself is lazy, so it can handle non terminating programs without
issue. What you do to get the output in that case is to ask it for a finite amount
of output, such as @take 10000 (interpret program)@. After evaluating enough to give
you @10000@ trace items, it will not evaluate more. -}
interpret :: InterpretConfig -> T.Trace
interpret config = runST interpret'
 where
  p = program config

  interpret' :: ST s T.Trace
  interpret' = do
      -- Fetch procedure body
    fun <- case Map.lookup (entry p) (funs p) of
      Just p' -> return p'
      Nothing ->
        error $ "Interpreter error: cannot find entry point: " ++ entry p

    -- Set up initial activation record
    process <-
      mkProc (entry p) 0 32 0 Nothing
      <$> params p
      <#> Map.empty
      <#> Nothing
      <#> body fun

    -- Input references that were given to the program. We fetch them here and put
    -- them in the main state record so that we can print their state afterwards.
    let actualrefs = getReferences p $ variableStorage process

    -- Run the interpret action and produce it's output
    ((term, _), events) <- runWriterT $ runStateT
      run
      (interpState 0                             -- now
                   Map.empty                     -- events
                   0                             -- numevents
                   (IntMap.singleton 0 process)  -- ready queue
                   1                             -- numcontinuations
                   (funs p)                      -- procedures
                   actualrefs                    -- input references
                   process                       -- current process
                   (boundContQueueSize config)   -- bound on continuation queue
                   (boundEventQueueSize config)
      ) -- bound on event queue
    return $ T.Trace (fromHughes events)

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
        n  <- getNow
        tellEvent $ T.DriverEventQueueStatus es n
        nextEventTime >>= setNow
        tick >> runInstant

  -- | Applies all scheduled updates for the current instant, then 'runConts'.
  tick :: Interp s ()
  tick = do
    performEvents
    nc <- contQueueSize
    runConts

  -- | Pop and run processes from the ready queue until the queue is empty.
  runConts :: Interp s ()
  runConts = do
    cs <- contQueueSize
    unless (cs < 1) $ do
      p <- dequeue
      setCurrentProcess p
      tellEvent $ T.ActStepBegin $ procName p
      step
      runConts

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
      NewRef n _ e -> do
        newRef n e
        continue

      GetRef n t r -> do
        v <- readRef r
        newVar n v
        continue

      SetRef r e -> do
        writeRef (fst r) e
        continue

      SetLocal n t e2 -> do
        writeRef (getVarName n) e2
        continue

      If c thn els -> do
        b <- getBool <$> eval c
        pushInstructions $ if b then thn else els
        continue

      While c bdy -> do
        b <- getBool <$> eval c
        when b $ pushInstructions $ bdy ++ [stm]
        continue

      Skip        -> continue

      After d r v -> do
        d' <- getUInt64 <$> eval d
        v' <- eval v
        n' <- getNow
        scheduleEvent r (n' + d') v'
        continue

      Wait refs -> do
        wait refs
        yield

      Fork procs -> do
        setRunningChildren (length procs)
        parent <- addressToSelf
        pdeps  <- pds (length procs)
        forM_ (zip procs pdeps) $ \(f, (prio, dep)) -> fork f prio dep parent
        yield

 where
  -- | Convenience names for local control flow.
  continue, yield :: Interp s ()
  continue = step
  yield    = return ()
