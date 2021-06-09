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

-- | Time limit on how long the interpreter should attempt to evaluate a program
testTimeout :: Int
testTimeout = 15000000

linesOfContext :: Int
linesOfContext = 8


-- | Parse the output line by line. If parsing fails, report the line at which
-- failure takes place.
doParseOutput :: Monad m => Slug -> String -> QC.PropertyM m T.Output
doParseOutput sl outs = do
  cTrace <- go $ lines outs
  reportOnFail sl "executed.out" $ show cTrace
  return cTrace
 where
  go :: Monad m => [String] -> QC.PropertyM m T.Output
  go []       = return []
  go (x : xs) = case T.parseLine x of
    Just line -> do
      rest <- go xs
      return $ line : rest
    Nothing -> do
      QC.monitor $ QC.counterexample x
      fail "Parse error"

-- | Interpret a program and produce a (potentially trucated) output trace
--
-- The evaluation is functionally limited to the number of steps specified by
-- limit, but also time-limited using the timeout function.
doInterpret :: Slug -> Program -> Int -> QC.PropertyM IO T.Output
doInterpret sl program limit = do
  iTrace <- QC.run timeoutEval
  reportOnFail sl "interpreted.out" $ show iTrace
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

-- | Compare two traces, and fail if they differ
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
  go i ctx (t : ts) (t' : ts')
    | t == t' = go (i + 1) (t : ctx) ts ts'
    | otherwise = do
      QC.monitor $ QC.counterexample report
      reportOnFail sl "output.diff" report
      fail $ "Traces differ at line " ++ show i
   where
    report    = unlines $ preamble ++ beforeCtx ++ [diff] ++ afterCtx

    preamble  = ["Output differs:", ""]
    beforeCtx = reverse $ take linesOfContext $ zipWith addLine rnum before
    afterCtx  = take linesOfContext $ zipWith addLine lnum after

    diff      = addLine i $ infixJoin " /= " (rpad $ show t) (rpad $ show t')
    before    = map show ctx
    after =
      zipWith (infixJoin " || ") (map (rpad . show) ts) (map (rpad . show) ts')

    rnum = reverse [1 .. i - 1]
    lnum = [i + 1 ..]

  go i ctx ts ts' = do
    QC.monitor $ QC.counterexample report
    fail "Trace lengths differ"
   where
    report    = unlines $ preamble ++ beforeCtx ++ afterCtx

    preamble  = ["Lengths differ: " ++ tll ++ " /= " ++ tll', "", "tail:"]
    tll       = show $ i + length ts
    tll'      = show $ i + length ts'

    beforeCtx = reverse $ take linesOfContext $ zipWith addLine rnum before
    afterCtx  = []

    before    = map show ctx
    after =
      zipWith (infixJoin " || ") (map (rpad . show) ts) (map (rpad . show) ts')

    rnum = reverse [1 .. i - 1]
    lnum = [i ..]

{-- Diff output formatting helpers --}
-- | Width
diffColumnWidth :: Int
diffColumnWidth = 40

-- | Right pad a string with `diffColumnWidth` spaces
rpad :: String -> String
rpad s = take diffColumnWidth $ s ++ repeat ' '

-- | Join two strings with infix
infixJoin :: [a] -> [a] -> [a] -> [a]
infixJoin i l r = l ++ i ++ r

-- | Decorate string with tab-padded line number
addLine :: Show a => a -> [Char] -> [Char]
addLine num l = show num ++ ":\t" ++ l
