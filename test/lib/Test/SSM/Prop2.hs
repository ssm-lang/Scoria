module Test.SSM.Prop2 where

import           SSM.Core                       ( Program, C2 )
import           SSM.Compile

import           Test.SSM.QuickCheck.Generator  ( ) -- instance Arbitrary Program

import qualified Test.QuickCheck               as QC
import qualified Test.QuickCheck.Monadic       as QC

import System.Process
import GHC.IO.Exception

propCompiles :: Program C2 -> QC.Property
propCompiles program = QC.monadicIO $ doMake (toC2' program)

doMake :: String -> QC.PropertyM IO ()
doMake cSrc = do
    QC.run cleanBuildDir
    QC.run $ createSourceFile cSrc
    compileSourceFile

-- remove build artifacs from previous test
cleanBuildDir :: IO ()
cleanBuildDir = do
    (code, out, err) <- readProcessWithExitCode "rm" ["ssm-runtime/program", "ssm-runtime/program.c"] ""
    return ()

-- create source file to compile
createSourceFile :: String -> IO ()
createSourceFile cSrc = writeFile "ssm-runtime/program.c" cSrc

compileSourceFile :: QC.PropertyM IO ()
compileSourceFile = do
    (code, out, err) <- QC.run $ readProcessWithExitCode "gcc" args ""
    case code of
        ExitSuccess -> return ()
        ExitFailure _ -> fail err
  where
      args :: [String]
      args = [ "-o"
             , "ssm-runtime/program"
             , "-g"
             , "-Issm-runtime/include"
             , "-O"
             , "-Wall"
             , "-pedantic"
             , "-std=c99"
             , "ssm-runtime/src/ssm-mem.c"
             , "ssm-runtime/src/ssm-scheduler.c"
             , "ssm-runtime/src/ssm-top-act.c"
             , "ssm-runtime/program.c"
             ]

-- QC.monitor $ QC.whenFail $ do
--   createDirectoryIfMissing True $ reportDir slug
--   writeFile (reportDir slug </> fp) s

--   -- | Call make with target, and handle error(s), if any.
--   make t = do
--     (code, out, err) <- QC.run $ readProcessWithExitCode "make" (mkArgs t) ""
--     case code of
--       ExitSuccess   -> return out
--       ExitFailure c -> do
--         reportUnixError slug ("make" : mkArgs t) (c, out, err)
--         fail $ "Make error: " ++ t
