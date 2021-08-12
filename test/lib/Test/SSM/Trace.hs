{-# LANGUAGE ScopedTypeVariables #-}
module Test.SSM.Trace
  ( doParseOutput
  , doInterpret
  , doCompareTraces
  ) where


import           Control.Exception              ( SomeException(..)
                                                , evaluate
                                                , try
                                                )
import           Data.IORef                     ( IORef
                                                , modifyIORef
                                                , newIORef
                                                , readIORef
                                                )
import           Data.List                      ( isPrefixOf
                                                , union
                                                )
import           System.Timeout                 ( timeout )
import           Text.Read                      ( readMaybe )

import           SSM.Interpret                  ( InterpretConfig(..)
                                                , SSMProgram(..)
                                                , interpret
                                                )
import qualified SSM.Interpret.Trace           as Tr
import qualified SSM.Interpret.TraceParser     as TrP

import           SSM.Util.Default               ( Default(..) )

import qualified Test.QuickCheck               as QC
import qualified Test.QuickCheck.Monadic       as QC

import           Data.Functor                   ( (<&>) )
import           Test.SSM.Report                ( Slug(..)
                                                , reportOnFail
                                                )

import           Data.Algorithm.Diff            ( Diff(..)
                                                , getDiff
                                                )

-- | Time limit on how long the interpreter should evaluate a program, in
-- microseconds.
testTimeout :: Int
testTimeout = 15000000

-- | Number of lines of context shown for diffs.
linesOfContext :: Int
linesOfContext = 16

-- | Width of diff and line number columns.
diffColumnWidth, diffNumWidth :: Int
(diffColumnWidth, diffNumWidth) = (60, 8)

-- | Parse the output line by line; report line number upon failure.
doParseOutput :: Monad m => Slug -> String -> QC.PropertyM m Tr.Trace
doParseOutput slug outs = do
  cTrace <- go 1 $ lines outs
  reportOnFail slug "executed.out" $ show cTrace
  return cTrace
 where
  go :: Monad m => Int -> [String] -> QC.PropertyM m [Tr.Event]
  go _  []       = fail "Parse error: empty output"
  go ln (x : xs) = case TrP.parseEventS x of
    Just e
      | null xs -> if Tr.isTerminal e
        then return [e]
        else fail "Parse error: trace ended with non-terminal event"
      | otherwise -> go (ln + 1) xs <&> (e :)
    Nothing
      | -- Skip empty line
        null x -> go (ln + 1) xs
      | otherwise -> do
        QC.monitor $ QC.counterexample x
        fail $ "Parse error: line " ++ show ln

-- | Interpret a program and produce a (potentially trucated) output trace.
--
-- The evaluation is functionally limited to the number of steps specified by
-- limit, but also time-limited using the timeout function.
doInterpret
  :: SSMProgram a => Slug -> a -> Int -> (Int, Int) -> QC.PropertyM IO Tr.Trace
doInterpret slug program limit (actQueueSize, eventQueueSize) = do
  iTrace <- QC.run timeoutEval
  reportOnFail slug "interpreted.out" $ show iTrace
  return iTrace
 where
  timeoutEval :: IO Tr.Trace
  timeoutEval = do
    ref <- newIORef []
    xs' <- timeout testTimeout $ try $ evalTrace interpreted limit ref
    case xs' of
      Nothing         -> return ()
      Just (Right ()) -> return ()
      Just (Left (e :: SomeException)) ->
        -- Only try to parse first line (i.e., ignore stack trace)
        case readMaybe $ head $ take 1 $ lines $ show e of
          Just (t :: Tr.Event) -> modifyIORef ref (t :)
          Nothing -> modifyIORef ref (Tr.CrashUnforeseen (show e) :)
    reverse <$> readIORef ref

  interpreted :: Tr.Trace
  interpreted = interpret
    def { boundActQueueSize   = actQueueSize
        , boundEventQueueSize = eventQueueSize
        }
    program

  evalTrace :: Tr.Trace -> Int -> IORef [Tr.Event] -> IO ()
  evalTrace []       _   _   = return ()
  evalTrace _        0   _   = return ()
  evalTrace (x : xs) lim ref = do
    y <- evaluate x
    modifyIORef ref (y :)
    evalTrace xs (lim - 1) ref

-- | Compare two traces, and fail if they differ.
--
-- Note that the "Expected" argument should come first.
doCompareTraces :: Monad m => Slug -> Tr.Trace -> Tr.Trace -> QC.PropertyM m ()
doCompareTraces slug expected got
  | null after = return ()
  | otherwise = do
    QC.monitor $ QC.counterexample $ unlines report
    reportOnFail slug "output.diff" $ unlines report
    fail $ "Traces differ at line " ++ show diffLn
 where

  -- | Whether a two items match in a 'Diff'.
  isBoth :: Diff a -> Bool
  isBoth (Both _ _) = True
  isBoth _          = False

  -- | Before and after the first difference in the event trace.
  before, after :: [Diff Tr.Event]
  (before, after) = span isBoth $ getDiff expected' got'
   where
    (expected', got') = if last got == Tr.ExhaustedMicrotick
      then (init expected, init got)
      else (expected, got)

  -- | The line number where the first difference occurs.
  diffLn :: Int
  diffLn = length before + 1

  -- | 'before' and 'after' bound to 'linesOfContext' and annotated with line nums.
  beforeCtx, afterCtx :: [(Int, Diff Tr.Event)]
  beforeCtx = drop (diffLn - 1 - linesOfContext) $ zip [1 .. diffLn] before
  afterCtx  = take linesOfContext $ zip [diffLn ..] after

  -- | Format a diff with line number, for generating report.
  fmtDiffLn :: (Int, Diff Tr.Event) -> String
  fmtDiffLn (ln, d) = addLine ln $ fmtDiff d

  -- | Format a diff, for generating report.
  fmtDiff :: Diff Tr.Event -> String
  fmtDiff (Both e g) = "Both:     " ++ rpad (show e) ++ " ~~ " ++ show g
  fmtDiff (First  e) = "Expected: " ++ rpad (show e) ++ " <<"
  fmtDiff (Second e) = "Got:      " ++ rpad "" ++ " >> " ++ show e

  -- | Error reported to user.
  report :: [String]
  report =
    "Traces differ:"
      :  ""
      :  map fmtDiffLn beforeCtx
      ++ ["------  !!!!  ------"] -- Loudly indicate that differences begin here
      ++ map fmtDiffLn afterCtx

-- | Right pad a string with `diffColumnWidth` spaces.
rpad :: String -> String
rpad s = take diffColumnWidth $ s ++ repeat ' '

-- | Decorate line with padded line number.
addLine :: Show a => a -> String -> String
addLine num l
  | length (show num) + 2 >= diffNumWidth = show num ++ ": " ++ l
  | otherwise = take diffNumWidth (show num ++ ": " ++ repeat ' ') ++ l
