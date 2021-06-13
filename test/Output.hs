{-# LANGUAGE ScopedTypeVariables #-}
module Output
  ( doParseOutput
  , doInterpret
  , doCompareTraces
  ) where

import           LowCore                        ( Program )
import           LowInterpreter                 ( interpret )
import qualified Trace                         as T

import           Report                         ( Slug(..)
                                                , reportOnFail
                                                )
import qualified Test.QuickCheck               as QC
import qualified Test.QuickCheck.Monadic       as QC

import           Control.Exception              ( SomeException(..)
                                                , evaluate
                                                , try
                                                )
import           Data.IORef                     ( IORef
                                                , modifyIORef
                                                , newIORef
                                                , readIORef
                                                )
import           Data.List                      ( isPrefixOf )
import           System.Timeout                 ( timeout )

-- | Time limit on how long the interpreter should attempt to evaluate, in us.
testTimeout :: Int
testTimeout = 15000000

-- | Number of lines of context shown for diffs.
linesOfContext :: Int
linesOfContext = 8

-- | Width of diff columns.
diffColumnWidth :: Int
diffColumnWidth = 60

-- | Width of number columns.
diffNumWidth :: Int
diffNumWidth = 8

-- | Parse the output line by line. If parsing fails, report the line at which
-- failure takes place.
doParseOutput :: Monad m => Slug -> String -> QC.PropertyM m T.Output
doParseOutput sl outs = do
  cTrace <- go $ zip [1 ..] $ lines outs
  reportOnFail sl "executed.out" $ unlines $ map show cTrace
  return cTrace
 where
  go :: Monad m => [(Int, String)] -> QC.PropertyM m T.Output
  go []            = return []
  go ((l, x) : xs) = case T.parseLine x of
    Just line -> do
      rest <- go xs
      return $ line : rest
    Nothing -> do
      QC.monitor $ QC.counterexample x
      fail $ "Parse error: line " ++ show l

-- | Interpret a program and produce a (potentially trucated) output trace
--
-- The evaluation is functionally limited to the number of steps specified by
-- limit, but also time-limited using the timeout function.
doInterpret :: Slug -> Program -> Int -> QC.PropertyM IO T.Output
doInterpret sl program limit = do
  iTrace <- QC.run timeoutEval
  reportOnFail sl "interpreted.out" $ unlines $ map show iTrace
  return iTrace
 where
  timeoutEval :: IO T.Output
  timeoutEval = do
    ref <- newIORef []
    xs' <- timeout testTimeout $ try $ eval (interpret program) limit ref
    case xs' of
      Just (Left (e :: SomeException)) -> case show e of
        v
          | "eventqueue full" `isPrefixOf` v -> modifyIORef
            ref
            (T.EventQueueFull :)
          | "negative depth" `isPrefixOf` v -> modifyIORef ref
                                                           (T.NegativeDepth :)
          | "bad after" `isPrefixOf` v -> modifyIORef ref (T.BadAfter :)
          | "contqueue full" `isPrefixOf` v -> modifyIORef ref
                                                           (T.ContQueueFull :)
        _ -> modifyIORef ref (T.Crash :)
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
doCompareTraces :: Monad m => Slug -> T.Output -> T.Output -> QC.PropertyM m ()
doCompareTraces sl = go 1 []
 where
  go
    :: Monad m
    => Int
    -> [T.OutputEntry]
    -> T.Output
    -> T.Output
    -> QC.PropertyM m ()
  go _ _ [] [] = return ()
  go n ctx (t : ts) (t' : ts')
    | t == t' = go (n + 1) (t : ctx) ts ts'
    | otherwise = do
      QC.monitor $ QC.counterexample report
      reportOnFail sl "output.diff" report
      fail $ "Traces differ at line " ++ show n
   where
    report =
      unlines $ preamble ++ before n ctx ++ diff n t t' ++ after n ts ts'
    preamble = ["Output differs:", "", header]

  go n ctx ts ts' = do
    QC.monitor $ QC.counterexample report
    fail "Trace lengths differ"
   where
    report   = unlines $ preamble ++ before n ctx ++ after n ts ts'
    preamble = ["Lengths differ: " ++ tll ++ " =/= " ++ tll', "", header]
    tll      = show $ n + length ts
    tll'     = show $ n + length ts'

  {--- Diff outputting helpers ---}

  -- | Format column header for diff.
  header :: String
  header = lpadding ++ infixJoin "     " (rpad "Expected") (rpad "Got")
    where lpadding = replicate (diffNumWidth - 1) '-' ++ " "

  -- | Format before context for diff.
  before :: Int -> T.Output -> [String]
  before n bs = reverse $ take linesOfContext $ zipWith addLine ns ls
   where
    ns = reverse [1 .. n - 1]
    ls = map (++ "  =  ...") $ rpadOutput bs

  -- | Format differing line for diff.
  diff :: Int -> T.OutputEntry -> T.OutputEntry -> [String]
  diff n t t' =
    [addLine n $ infixJoin " =/= " (rpad $ show t) (rpad $ show t')]

  -- | Format after context for diff.
  after :: Int -> T.Output -> T.Output -> [String]
  after n ts ts' = take linesOfContext $ zipWith addLine [n + 1 ..] ls
    where ls = zipWith (infixJoin "  /  ") (rpadOutput ts) (rpadOutput ts')

  -- | Print and pad each line of an output.
  rpadOutput :: T.Output -> [String]
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
