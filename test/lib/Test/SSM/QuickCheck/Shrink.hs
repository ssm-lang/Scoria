module Test.SSM.QuickCheck.Shrink where

import SSM.Core.Syntax ( Program )

import Test.SSM.QuickCheck.Shrink.Procedures
    ( shrinkSingleProcedures,
      removeUnusedProcedures,
      shrinkManyProcedures )
import Test.SSM.QuickCheck.Shrink.References ( refs )
import Test.SSM.QuickCheck.Shrink.If ( ifs )
import Test.SSM.QuickCheck.Shrink.Wait ( waits )
import Test.SSM.QuickCheck.Shrink.Fork ( forks )
import Test.SSM.QuickCheck.Shrink.ProcedureArity ( arities )
import Test.SSM.QuickCheck.Shrink.Statements ( statements )
import Test.SSM.QuickCheck.Shrink.Expressions


shrinkProgram :: Program -> [Program]
shrinkProgram p =
    let p' = removeUnusedProcedures p
    in concat $ map ($p') [ shrinkManyProcedures
                          , shrinkSingleProcedures
                          , arities
                          , statements
                          , forks
                          , ifs
                          , refs
                          , waits
                          , expressions
                          ]
