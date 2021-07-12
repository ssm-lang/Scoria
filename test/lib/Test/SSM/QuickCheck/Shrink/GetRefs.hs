module Test.SSM.QuickCheck.Shrink.GetRefs
    ( getrefs ) where

import SSM.Core.Syntax
import SSM.Util.HughesList hiding ( (++) )

import Test.SSM.QuickCheck.Util

import qualified Data.Map as Map

import Debug.Trace

{- | Shrink a program into many mutations by removing every `GetRef` statement in
the program. Since a `GetRef` creates a new expression variable the rest of the
program must be altered to contain no references to this newly created expression
variable. And as the rest of the program is pruned by the utility `removeVars` function
depending on what is in scope, we must track which references are valid so that we can
pass that in to the function.

If a program contain 3 `GetRef` statements, `getrefs` will return 3 mutations where each
mutation has only 2 `GetRef` statements. -}
getrefs :: Program -> [Program]
getrefs p = concat
    [ 
      let validrefs = map (uncurry makeDynamicRef) $ filter (isReference . snd) (arguments procedure)
      in [ p { funs = Map.insert n (procedure { body = body' }) (funs p) }
         | body' <- shrinkProcedureBody validrefs (body procedure)
         ] 

    | (n,procedure) <- Map.toList (funs p)
    ]

{- | Need to write this explicitly rather than using distributeMutate, as the
mutation is not local to just the GetRef statement itself. -}
shrinkProcedureBody :: [Reference] -> [Stm] -> [[Stm]]
shrinkProcedureBody validrefs xs = go (emptyHughes, xs, validrefs)
  where
      go :: (Hughes Stm, [Stm], [Reference]) -> [[Stm]]
      go (_, [], _)                   = []
      go (current, (x:xs), validrefs) = case x of
          NewRef n t e ->
              go (snoc current x, xs, makeDynamicRef (getVarName n) t:validrefs)
          GetRef n t r ->
              let rest = removeVars [getVarName n] validrefs (x:xs)
              in (fromHughes current <> rest) :
                 go (snoc current x, xs, validrefs)
          If c thn els ->
              let thns  = go (emptyHughes, thn, validrefs)
                  elss  = go (emptyHughes, els, validrefs)
                  front = fromHughes current
              in [ front ++ (If c thn' els:xs) | thn' <- thns ] ++
                 [ front ++ (If c thn els':xs) | els' <- elss ] ++
                 go (snoc current x, xs, validrefs)
          While c bdy  ->
              let bdys  = go (emptyHughes, bdy, validrefs)
                  front = fromHughes current
              in [ front ++ (While c bdy':xs) | bdy' <- bdys ] ++
                 go (snoc current x, xs, validrefs)
          _            -> go (snoc current x, xs, validrefs)
