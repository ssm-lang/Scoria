{-# LANGUAGE ScopedTypeVariables #-}
module Test.SSM.Output
  ( doParseOutput
  , doInterpretWithSize
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

import           SSM.Core.Syntax                ( Program )
import           SSM.Interpret                  ( interpret
                                                , customQueueSizes
                                                )
import qualified SSM.Interpret.Trace           as Tr

import qualified Test.QuickCheck               as QC
import qualified Test.QuickCheck.Monadic       as QC

import           Test.SSM.Report                ( Slug(..)
                                                , reportOnFail
                                                )

-- | Time limit on how long the interpreter should evaluate a program, in
-- microseconds.
testTimeout :: Int
testTimeout = 15000000

-- | Number of lines of context shown for diffs.
linesOfContext :: Int
linesOfContext = 5

-- | Width of diff columns.
diffColumnWidth :: Int
diffColumnWidth = 60

-- | Width of number columns.
diffNumWidth :: Int
diffNumWidth = 8

-- | List to store event queue sizes for testing
queueSizes :: [(Int, Int)]
queueSizes = [(32, 32), (256, 256), (2048, 2048)]

-- | Parse the output line by line. If parsing fails, report the line at which
-- failure takes place.
doParseOutput :: Monad m => Slug -> String -> QC.PropertyM m Tr.Output
doParseOutput slug outs = do
  cTrace <- go $ zip [1 ..] $ lines outs
  reportOnFail slug "executed.out" $ unlines $ map show cTrace
  return cTrace
 where
  go :: Monad m => [(Int, String)] -> QC.PropertyM m Tr.Output
  go []            = return []
  go ((l, x) : xs) = case Tr.parseLine x of
    Just line -> do
      rest <- go xs
      return $ line : rest
    Nothing -> do
      QC.monitor $ QC.counterexample x
      fail $ "Parse error: line " ++ show l

-- | Interpret a program and produce a (potentially trucated) output trace.
--
-- The evaluation is functionally limited to the number of steps specified by
-- limit, but also time-limited using the timeout function.
doInterpretWithSize :: Slug -> Program -> Int -> QC.PropertyM IO Tr.Output
doInterpretWithSize slug program limit = do
  iTrace <- QC.run timeoutEval
  reportOnFail slug "interpreted.out" $ unlines $ map show iTrace
  return iTrace
 where
  timeoutEval :: IO Tr.Output
  timeoutEval = do
    ref <- newIORef []
    xs' <- timeout testTimeout $ try $ eval (interpret (customQueueSizes 20 20 program)) limit ref
    case xs' of
      Just (Left (e :: SomeException)) -> case show e of
        v
          | "eventqueue full" `isPrefixOf` v -> modifyIORef
            ref
            (Tr.EventQueueFull :)
          | "negative depth" `isPrefixOf` v -> modifyIORef
            ref
            (Tr.NegativeDepth :)
          | "bad after" `isPrefixOf` v -> modifyIORef ref (Tr.BadAfter :)
          | "contqueue full" `isPrefixOf` v -> modifyIORef
            ref
            (Tr.ContQueueFull :)
        _ -> modifyIORef ref (Tr.Crash :)
      _ -> return ()
    reverse <$> readIORef ref

  eval :: [a] -> Int -> IORef [a] -> IO ()
  eval []       _   _   = return ()
  eval _        0   _   = return ()
  eval (x : xs) lim ref = do
    y <- evaluate x
    modifyIORef ref (y :)
    eval xs (lim - 1) ref

-- | Compare two traces, and fail if they differ.
--
-- Note that the "Expected" argument should come first.
doCompareTraces
  :: Monad m => Slug -> Tr.Output -> Tr.Output -> QC.PropertyM m ()
doCompareTraces slug = go 1 ([], [])
 where
  go
    :: Monad m
    => Int
    -> ([Tr.OutputEntry], [Tr.OutputEntry])
    -> Tr.Output
    -> Tr.Output
    -> QC.PropertyM m ()
  go _ _ [] [] = return ()

  -- | Events in the same instant may be applied out of order, so we need to
  -- relax the trace comparison and only ensure the same set of events are being
  -- applied. Furthermore, we need to account for possible crashes, and
  -- differences in crash timing due to event application order.
  go n (bs, bs') tts@(Tr.Event _ _ : _) tts'@(Tr.Event _ _ : _)
    | incomplete = return ()
    | crashed = return ()
    | eventSetsEq = go (n + length es)
                       (reverse es ++ bs, reverse es' ++ bs')
                       ts
                       ts'
    | otherwise = do
      QC.monitor $ QC.counterexample report
      reportOnFail slug "output.diff" report
      fail $ "Traces differ in event set starting at line " ++ show n
   where
    (es , ts )  = span (\e -> isEvent e || isCrash e) tts
    (es', ts')  = span (\e -> isEvent e || isCrash e) tts'

    -- | Whether the trace stops abrupty after the event set.
    --
    -- This will happen if the trace terminates prematurely, due to running out
    -- of microticks or timing out.
    incomplete = null ts && null ts'

    -- | Whether either trace ends with a crash.
    crashed     = isCrash (last es) || isCrash (last es')

    -- | Whether two event sets are set-wise equivalent.
    eventSetsEq = length ese == length es && length ese == length es'
      where ese = es `union` es'

    isEvent (Tr.Event _ _) = True
    isEvent _              = False

    isCrash Tr.Crash          = True
    isCrash Tr.EventQueueFull = True
    isCrash Tr.ContQueueFull  = True
    isCrash Tr.NegativeDepth  = True
    isCrash Tr.BadAfter       = True
    isCrash _                 = False

    report = unlines $ preamble ++ before n bs bs' ++ diff len n tts tts'
     where
      preamble = ["Event sets differ:", "", header]
      len      = length es `max` length es'

  go n (bs, bs') (t : ts) (t' : ts')
    | t == t' = go (n + 1) (t : bs, t' : bs') ts ts'
    | otherwise = do
      QC.monitor $ QC.counterexample report
      reportOnFail slug "output.diff" report
      fail $ "Traces differ at line " ++ show n
   where
    report =
      unlines $ preamble ++ before n bs bs' ++ diff 1 n (t : ts) (t' : ts')
    preamble = ["Traces differ:", "", header]

  go n (bs, bs') ts ts' = do
    QC.monitor $ QC.counterexample report
    fail "Trace lengths differ"
   where
    report   = unlines $ preamble ++ before n bs bs' ++ diff 0 n ts ts'
    preamble = ["Lengths differ: " ++ tll ++ " =/= " ++ tll', "", header]
    tll      = show $ n + length ts
    tll'     = show $ n + length ts'

  {--- Diff outputting helpers ---}

  -- | Format column header for diff.
  header :: String
  header = lpadding ++ infixJoin "     " (rpad "Expected") (rpad "Got")
    where lpadding = replicate (diffNumWidth - 1) '-' ++ " "

  -- | Format before context for diff, in reverse.
  before :: Int -> [Tr.OutputEntry] -> [Tr.OutputEntry] -> [String]
  before n ts ts' = reverse $ take linesOfContext $ joinColumns
    "  ~  "
    (reverse [1 .. n - 1])
    ts
    ts'

  -- | Format differing line for diff, and some lines of context after.
  diff :: Int -> Int -> [Tr.OutputEntry] -> [Tr.OutputEntry] -> [String]
  diff len n ts ts' =
    take (len + linesOfContext) $ joinColumns " =/= " [n ..] ts ts'

  -- | Format columns for diff, separated by sep and with leading line numbers
  joinColumns
    :: String -> [Int] -> [Tr.OutputEntry] -> [Tr.OutputEntry] -> [String]
  joinColumns sep ns ts ts' = zipWith addLine ns ls
    where ls = zipWith (infixJoin sep) (rpadOutput ts) (rpadOutput ts')

  -- | Print and pad each line of an output.
  rpadOutput :: Tr.Output -> [String]
  rpadOutput ts = map (rpad . show) ts

  -- | Right pad a string with `diffColumnWidth` spaces.
  rpad :: String -> String
  rpad s = take diffColumnWidth $ s ++ repeat ' '

  -- | Join two strings with infix separator.
  infixJoin :: [a] -> [a] -> [a] -> [a]
  infixJoin i l r = l ++ i ++ r

  -- | Decorate line with padded line number.
  addLine :: Show a => a -> [Char] -> [Char]
  addLine num l
    | length (show num) + 2 >= diffNumWidth = show num ++ ": " ++ l
    | otherwise = take diffNumWidth (show num ++ ": " ++ repeat ' ') ++ l
