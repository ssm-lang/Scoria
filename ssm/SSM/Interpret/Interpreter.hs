module SSM.Interpret.Interpreter ( interpret ) where

import SSM.Util.HughesList
import SSM.Interpret.API
import SSM.Interpret.Exp
import SSM.Core.LowSyntax
import qualified SSM.Interpret.Trace as T

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap

import Control.Monad
import Control.Monad.Writer.Lazy
import Control.Monad.State.Lazy
import Control.Monad.ST.Lazy

interpret :: Program -> T.Output
interpret p = runST interpret'
  where
      interpret' :: ST s T.Output
      interpret' = do
          -- Fetch procedure body
          fun <- case Map.lookup (entry p) (funs p) of
              Just p'  -> return p'
              Nothing  -> error $ concat ["interpreter error - can not find function ", entry p]
          
          -- Set up initial activation record
          process <- mkProc 0 32 0 Nothing <$> params p <#> Map.empty <#> Nothing <#> body fun

          -- Input references that were given to the program. We fetch them here and put
          -- them in the main state record so that we can print their state afterwards.
          let actualrefs = getReferences p $ variableStorage process
          
          -- Run the interpret action and produce it's output
          outp <- execWriterT $ 
                  evalStateT (run >> emitResult) 
                      (interpState
                          0                             -- now
                          Map.empty                     -- events
                          0                             -- numevents
                          (IntMap.singleton 0 process)  -- ready queue
                          1                             -- numcontinuations
                          (funs p)                      -- procedures
                          actualrefs                    -- input references
                          process)                      -- current process
          return $ fromHughes outp

-- | Run the interpreter. This is quivalent to the `main` function generated by the code generator.
run :: Interp s ()
run = do tick
         c  <- now
         es <- eventQueueSize
         tell $ toHughes [T.Instant c es]
         instant
  where
      -- | Runs one instant. It will advance model time to the next event time, and
      -- then call tick.
      instant :: Interp s ()
      instant = do
        b <- eventQueueEmpty
        if b
            then return ()
            else do now' <- nextEventTime
                    setNow now'
                    tick

                    c  <- now
                    es <- eventQueueSize 
                    tell $ toHughes [T.Instant c es]
                    instant
    
      tick :: Interp s ()
      tick = do
          performEvents
          nc <- contQueueSize 
          tell $ toHughes [T.NumConts nc]
          mainloop

-- | This function will keep popping processes off the ready queue, running them and then
-- calling itself again until the ready queue is empty.
mainloop :: Interp s ()
mainloop = do
    cs <- contQueueSize
    if cs == 0
        then return ()
        else do p <- dequeue
                setCurrentProcess p
                step
                mainloop

-- | Run instructions of a process until the process should terminate (there are no
-- instructions left) or it should block (by evaluating a wait or fork instruction).
step :: Interp s ()
step = do
    i <- nextInstruction
    p <- currentProcess
    case i of

        Nothing -> leave

        Just stm -> case stm of
            NewRef n _ e    -> do
                newRef n e
                next
            GetRef n t r    -> do
                v <- readRef r
                newVar n v
                next
            SetRef r e      -> do
                writeRef (fst r) e
                next
            SetLocal n t e2 -> do
                writeRef (getVarName n) e2
                next
            If c thn els    -> do
                b <- getBool <$> eval c
                pushInstructions $ if b then thn else els
                next
            While c bdy     -> do
                b <- getBool <$> eval c
                if b
                    then pushInstructions $ bdy ++ [stm]
                    else return ()
                next
            Skip            -> next
            After d r v     -> do
                d'    <- getUInt64 <$> eval d
                v'    <- eval v
                now'  <- now
                schedule_event r (now' + d') v'
                next

            Wait refs       -> do
                wait refs
                yield

            Fork procs      -> do
                tell $ toHughes [T.Fork $ map fst procs]

                setRunningChildren (length procs)
                parent <- addressToSelf
                pdeps  <- pds (length procs)
                forM_ (zip procs pdeps) $ \(f,(prio, dep)) -> do
                    fork f prio dep parent

                yield


yield :: Interp s ()
yield = return ()

next :: Interp s ()
next = step