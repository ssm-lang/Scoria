{- | This module implements the `Waitable` typeclass. The typeclass exposes a function
`wait` which emits a blocking instruction.

There are instances for this typeclass for tuples of size @0@ to @62@. These are
automatically generated using Template Haskell.

assuming @r1@, @r2@, and @r3@ are of type @Ref a@ where the type varaible @a@ is
different for each reference, all the following calls are legal

@
> wait r1
> wait r2
> wait (r1,r2)
> wait (r2,r1)
> wait (r1,r2,r3)
@
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module SSM.Frontend.Waitable
    ( Waitable(..)
    , makeWaitableInstance
    ) where

import           SSM.Core.Syntax         hiding ( Wait )
import           SSM.Frontend.Ref
import           SSM.Frontend.Syntax

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

-- | Class of types that can be waited for
class Waitable a where
    wait :: a -> SSM ()

-- | @makeWaitableInstance n@ creates an instance of `Waitable` for tuples of size @n@
makeWaitableInstance :: Int -> Dec
makeWaitableInstance n = InstanceD Nothing [] waitabledecl [waitableequation]
  where
    waitabledecl :: Language.Haskell.TH.Type
    waitabledecl = AppT (ConT $ mkName "Waitable") $ tupletype n

    waitableequation :: Dec
    waitableequation =
        FunD (mkName "wait") [Clause [tuplepattern n] (NormalB $ equation n) []]

-- | Create a tuple type of size @n@, where each @Ref a@ has a different parameter @a@
tupletype :: Int -> Language.Haskell.TH.Type
tupletype n = foldl AppT (TupleT n) types
  where
    types :: [Language.Haskell.TH.Type]
    types =
        [ AppT (ConT $ mkName "Ref") (VarT $ mkName $ "a" ++ show i)
        | i <- [1 .. n]
        ]

-- | Create the left-hand side arguments of a `wait` equation
tuplepattern :: Int -> Pat
tuplepattern n = TupP vars
  where
    vars :: [Pat]
    vars =
        [ ConP (mkName "Ptr") [VarP (mkName $ "r" ++ show i)] | i <- [1 .. n] ]

-- | Create the right-hand side of a `wait` equation
equation :: Int -> Exp
equation n = AppE (VarE $ mkName "emit") $ AppE (ConE $ mkName "Wait") $ ListE
    vars
  where
    vars :: [Exp]
    vars = [ VarE $ mkName $ "r" ++ show i | i <- [1 .. n] ]
