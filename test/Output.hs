{-# LANGUAGE ScopedTypeVariables #-}
module Output
  ( doParseOutput
  , doInterpret
  , doCompareTraces
  ) where

import           LowCore                        ( Program )
import           LowInterpreter                 ( interpret )
import qualified Trace                         as T

import           Report                         ( reportOnFail )
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

-- | Parse the output line by line. If parsing fails, report the line at which
-- failure takes place.
doParseOutput :: Monad m => String -> QC.PropertyM m T.Output
doParseOutput outs = do
  cTrace <- go $ lines outs
  reportOnFail "executed.out" $ show cTrace
  return cTrace
 where
  go :: Monad m => [String] -> QC.PropertyM m T.Output
  go []       = return []
  go (x : xs) = case T.parseLine x of
    Just line -> do
      rest <- go xs
      return $ line : rest
    Nothing -> reportParseError >> fail "Parse error"
    where reportParseError = QC.monitor $ QC.counterexample x

-- | Interpret a program and produce a (potentially trucated) output trace
--
-- The evaluation is functionally limited to the number of steps specified by
-- limit, but also time-limited using the timeout function.
doInterpret :: Program -> Int -> QC.PropertyM IO T.Output
doInterpret program limit = do
  iTrace <- QC.run timeoutEval
  reportOnFail "interpreted.out" $ show iTrace
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
doCompareTraces :: Monad m => T.Output -> T.Output -> QC.PropertyM m ()
doCompareTraces = go 1
 where
  go :: Monad m => Int -> T.Output -> T.Output -> QC.PropertyM m ()
  go _ [] [] = return ()
  go i (t : ts) (t' : ts')
    | t == t' = go (i + 1) ts ts'
    | otherwise = do
      QC.monitor $ QC.counterexample diff
      fail $ "Traces differ at line " ++ show i
    where diff = show i ++ ":   " ++ show t ++ " /= " ++ show t'
  go i ts ts' = fail $ "Trace lengths differ: " ++ diff
   where
    tsl  = show $ i + length ts
    tsl' = show $ i + length ts'
    diff = tsl ++ " /= " ++ tsl'
