{-# LANGUAGE LambdaCase #-}
module Test.SSM.QuickCheck.Shrink.ProcedureArity where

import SSM.Core.Syntax

import Test.SSM.QuickCheck.Util

import Data.List
import Data.Maybe
import Data.Either

import qualified Data.Map as Map

import Debug.Trace

-- | Shrink procedure arities
arities :: Program -> [Program]
arities p =
  [ -- delete argument from argument list
    let newargs     = removeNth i (arguments procedure)
        -- rewrite the procedure body of n
        procedure'  = shrinkProcedureBody (fst arg) $
                        shrinkProcedureCalls i n (procedure { arguments = newargs })
        -- rewrite all other procedure bodies to contain no reference to n
        procedures' = map (shrinkProcedureCalls i n) $
                        Map.elems $ Map.delete n (funs p)
        -- create the new procedure map
        funs'       = Map.fromList $ map (\p -> (name p, p)) $ procedure':procedures'

    {- update the program to contain the new procedures. If the shrunk procedure was
    the entry point, remove the ith argument from it. -}
    in if n == (entry p)
          then p { args = removeNth i (args p)
                 , funs = funs'}
          else p { funs = funs' }

  | (n,procedure) <- Map.toList $ funs p
  , (i,arg)       <- zip [0..] (arguments procedure)
  ]

{- | Remove the @i@th parameter from a procedure. The procedure body is rewritten to
contain no references to the deleted parameter, and any expression that depended on
that parameter has been either rewritten or deleted. -}
shrinkProcedureBody :: Ident      -- ^ Name of parameter to remove
                    -> Procedure  -- ^ Procedure that it was removed from
                    -> Procedure
shrinkProcedureBody n p = p { body = removeVars [n] refs (body p) }
  where
    refs = filter (isReference . snd) (arguments p)

{- | If a procedure @n@ has had its @i@th parameter removed, this function will remove
the @i@th arguments from calls to @n@ from any other procedure @m@, and @m@ might
event be the same procedure @n@. -}
shrinkProcedureCalls :: {- Index in the parameter list of of the parameter that was
                        deleted from the procedure @n@. -}
                        Int
                     -> Ident      -- ^ name of procedure @n@
                     -> Procedure  -- ^ procedure @m@ to alter to reflect the changes
                     -> Procedure
shrinkProcedureCalls i n p = p { body = removeCalls i (body p) }
  where

    -- | Remove the @i@th argument from procedure calls to procedure @n@.
    removeCalls :: Int -> [Stm] -> [Stm]
    removeCalls _ []     = []
    removeCalls i xs = for xs $ \case

      If c thn els -> If c (removeCalls i thn) (removeCalls i els)

      While c bdy  -> While c (removeCalls i bdy)

      Fork procs   -> Fork $ for procs $ \(fun,args) ->
                        if fun == n
                          then (fun, removeNth i args)
                          else (fun, args)

      x            -> x
