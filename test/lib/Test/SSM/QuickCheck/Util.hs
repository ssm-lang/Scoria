{- | This module exposes some helpful utility functions used by the quickcheck
machinery. -}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.SSM.QuickCheck.Util
    ( transformProcedures
    , distributeMutate
    , for
    , removeNth
    , filterWith
    , removeVars
    ) where

import SSM.Core.Syntax
import SSM.Util.HughesList hiding ( (++) )

import qualified Data.Map as Map

{- | This function takes a function that returns a list of shrunk `Procedure`s, and
applies it to all procedures in the `Program`. For each new mutated procedure, a
new program is produced. In each new `Program`, only one procedure has been mutated.

Example: You have a program with 5 procedures and they can all be shrunk twice each. The
result of calling @transformProcedures@ will be a list of 10 new programs. -}
transformProcedures :: (Procedure -> [Procedure]) -> Program -> [Program]
transformProcedures tr prg = [ prg { funs = Map.insert n fun' (funs prg) }
                             | (n,fun) <- Map.toList (funs prg)
                             , fun' <- tr fun
                             ]

{- | Distribute a mutation function over a list of elements.

Example:
>>> distributeMutate [1,2,3] (\x -> if even x then [5,6] else [])
>>> [[1,5,3], [1,6,3]]
-}
distributeMutate :: forall a . [a] -> (a -> [a]) -> [[a]]
distributeMutate xs f = map fromHughes $ go (emptyHughes, xs)
  where
      go :: (Hughes a, [a]) -> [Hughes a]
      go (_, [])           = []
      go (current, (y:ys)) = [ current <> (cons_ y' ys) | y' <- f y ] ++
                             go (snoc current y, ys)

{- | Flip the arguments to map - in some cases it's a bit nicer on the eyes. Especially
in the function is more than a few characters big. -}
for :: [a] -> (a -> b) -> [b]
for = flip map

-- | Remove the n:th element from a list, with the first element being indexed as 0.
removeNth :: Show a => Int -> [a] -> [a]
removeNth 0 (_:xs) = xs
removeNth n (x:xs) = x : removeNth (n-1) xs
removeNth _ []     = error "can not remove from empty list"

-- | Like normal filter, but applies a function to the filtered elements
filterWith :: (a -> Bool) -> (a -> b) -> [a] -> [b]
filterWith _ _ []        = []
filterWith pred f (x:xs) =
    if pred x
        then f x : filterWith pred f xs
        else filterWith pred f xs



{- | Shrinks a sequence of statements by removing declarations. The first argument list
contains variable names that should be removed. If a `NewRef` or `GetRef` constructor
is observed which creates a variable with a name that's an element of that list,
it is removed. The second list contains those references that are still valid and
possible to use.

-}
removeVars :: [Ident] -> [(Ident, Type)] -> [Stm] -> [Stm]
removeVars = go
  where
      -- | Transforms a procedure body to contain no reference of the invalid identifiers
      go :: [Ident]          -- ^ Names of invalid identifiers
         -> [(Ident, Type)]  -- ^ Names & Types of valid references
         -> [Stm]            -- ^ Statements to transform
         -> [Stm]
      go _ _ []           = []
      go invalid validrefs (x:xs) = case x of

        NewRef n t e   ->
            if n `elem` invalid
                then Skip : go invalid validrefs xs
                else NewRef n t (rewriteExp e invalid validrefs) :
                     go invalid ((n, t):validrefs) xs

        GetRef n t r   ->
            if n `elem` invalid
                then Skip : go invalid validrefs xs
                else
            if not $ (fst r, refType r) `elem` validrefs
                then Skip : go (n:invalid) validrefs xs
                else x : go invalid validrefs xs

        SetRef r e     ->
            if not $ (fst r, refType r) `elem` validrefs
                then Skip : go invalid validrefs xs
                else SetRef r (rewriteExp e invalid validrefs) :
                     go invalid validrefs xs

        SetLocal n t e ->
            if n `elem` invalid
                then Skip : go invalid validrefs xs
                else SetLocal n t (rewriteExp e invalid validrefs) :
                     go invalid validrefs xs

        If c thn els   ->
            let thn' = go invalid validrefs thn
                els' = go invalid validrefs els
            in If (rewriteExp c invalid validrefs) thn' els' :
                  go invalid validrefs xs

        While c bdy    ->
            let bdy' = go invalid validrefs bdy
            in While (rewriteExp c invalid validrefs) bdy' :
               go invalid validrefs xs

        Skip           -> Skip : go invalid validrefs xs

        After d r v    ->
            if not $ (fst r, refType r) `elem` validrefs
                then Skip : go invalid validrefs xs
                else After (rewriteExp d invalid validrefs)
                           r
                           (rewriteExp v invalid validrefs) :
                     go invalid validrefs xs

        Wait refs      ->
            let refs' = filter (\r -> (fst r, refType r) `elem` validrefs) refs
            in if null refs'
                then Skip : go invalid validrefs xs
                else Wait refs' : go invalid validrefs xs

        Fork procs     ->
            let procs' = concat $ for procs $ \(n,args) ->
                  case rewriteCall args invalid validrefs of
                      Just args' -> [(n, args')]
                      Nothing    -> []
            in if null procs'
                then Skip : go invalid validrefs xs
                else Fork procs' : go invalid validrefs xs

      {- | Rewrite an expression by replacing invalid expressions with fresh literals
      and my replacing invalid references with a `True` literal. -}
      rewriteExp :: SSMExp -> [Ident] -> [(Ident, Type)] -> SSMExp
      rewriteExp e invalid validrefs = case e of
          Var t n        -> if n `elem` invalid
              then defaultExp t
              else Var t n
          Lit t l        -> Lit t l
          UOpE t e op    -> case op of
              Neg -> UOpE t (rewriteExp e invalid validrefs) Neg
          UOpR t r op    -> case op of
              Changed -> if (fst r, refType r) `elem` validrefs
                  then UOpR t r Changed
                  else Lit TBool (LBool True)
          BOp t e1 e2 op ->
              BOp t
                  (rewriteExp e1 invalid validrefs)
                  (rewriteExp e2 invalid validrefs)
                  op

      -- | Default literal of expressions
      defaultExp :: Type -> SSMExp
      defaultExp TInt32  = Lit TInt32 (LInt32 1)
      defaultExp TInt64  = Lit TInt64 (LInt64 1)
      defaultExp TUInt8  = Lit TUInt8 (LUInt8 1)
      defaultExp TUInt64 = Lit TUInt64 (LUInt64 1)
      defaultExp TBool   = Lit TBool (LBool True)
      defaultExp (Ref _) = error "can not work on reference types"

      {- | Try to rewrite the arguments to a procedure call. If there's a reference
      that's no longer valid that can't be replaced with a reference of the same type,
      that specific procedure call can't be left in the program. -}
      rewriteCall :: [Either SSMExp Reference]
                  -> [Ident]
                  -> [(Ident, Type)]
                  -> Maybe [Either SSMExp Reference]
      rewriteCall [] _ _                         = Just []

      rewriteCall (Left e:xs) invalid validrefs  = do
          xs' <- rewriteCall xs invalid validrefs
          return $ Left (rewriteExp e invalid validrefs) : xs'

      rewriteCall (Right r:xs) invalid validrefs = do
          xs' <- rewriteCall xs invalid validrefs
          if (fst r, refType r) `elem` validrefs
              then return $ Right r : xs'
              else do r' <- replacementRef r validrefs
                      return $ Right r' : xs'

      {- | If there's a ref of the same type in scope, return one of them. Otherwise,
      return Nothing. -}
      replacementRef :: Reference -> [(Ident, Type)] -> Maybe Reference
      replacementRef r refs =
          let refs' = filter (\(_,t) -> refType r == t) refs
          in if null refs' then Nothing else Just (head refs')
