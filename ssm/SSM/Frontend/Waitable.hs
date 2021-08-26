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

class Waitable a where
    wait :: a -> SSM ()

makeWaitableInstance :: Int -> Dec
makeWaitableInstance n = InstanceD Nothing [] waitabledecl [waitableequation]
  where
    waitabledecl :: Language.Haskell.TH.Type
    waitabledecl = AppT (ConT $ mkName "Waitable") $ tupletype n

    waitableequation :: Dec
    waitableequation =
        FunD (mkName "wait") [Clause [tuplepattern n] (NormalB $ equation n) []]

tupletype :: Int -> Language.Haskell.TH.Type
tupletype n = foldl AppT (TupleT n) types
  where
    types :: [Language.Haskell.TH.Type]
    types =
        [ AppT (ConT $ mkName "Ref") (VarT $ mkName $ "a" ++ show i)
        | i <- [1 .. n]
        ]

tuplepattern :: Int -> Pat
tuplepattern n = TupP vars
  where
    vars :: [Pat]
    vars =
        [ ConP (mkName "Ptr") [VarP (mkName $ "r" ++ show i)] | i <- [1 .. n] ]

equation :: Int -> Exp
equation n = AppE (VarE $ mkName "emit") $ AppE (ConE $ mkName "Wait") $ ListE
    vars
  where
    vars :: [Exp]
    vars = [ VarE $ mkName $ "r" ++ show i | i <- [1 .. n] ]
