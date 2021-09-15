{- | This module defines a type class @CPeripheral@ that can be used to generate C code
for a IO peripheral. Every peripheral should be made an instance of this type class to
facilitate easier code generation. -}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module SSM.Backend.C.Peripheral where

import           SSM.Core.Program
import           SSM.Core.Peripheral
import           SSM.Core.Reference
import           SSM.Core.Type

import           SSM.Backend.C.Identifiers

import           Language.C.Quote.GCC
import qualified Language.C.Syntax             as C

decls :: Peripheral -> [C.Definition]
decls (Peripheral a) = map declSingle $ declaredReferences a
  where
    declSingle :: Reference -> C.Definition
    declSingle r =
      [cedecl| $ty:(svt_ $ dereference $ refType r) $id:(refName r);|]

maininit :: Peripheral -> [C.BlockItem]
maininit (Peripheral a) = concatMap compInitializer $ mainInitializers a
    where
      compInitializer :: Initializer -> [C.BlockItem]
      compInitializer i = case i of
        Normal ref ->
          let bt = dereference $ refType ref
          in  [ [citem| $id:(initialize_ bt)(&$id:(refName ref));|]
              , [citem| $id:(assign_ bt)(&$id:(refName ref), 0, 0); |]
              ]
        StaticInput si ref -> case si of
          Switch id ->
            [ [citem| $id:initialize_static_input_device(&$id:(refName ref), $int:id);|]
            ]

{- | Item to be placed in a HList, where the only thing we know about the internal object
is that it has an instance of `CPeripheral`. -}
--data CPeripherable where
--    Peripheral ::CPeripheral a => a -> CPeripherable

-- | Return all the statements that initialize the peripherals statically
initPeripherals :: Program -> [C.BlockItem]
initPeripherals p = concatMap maininit $ peripherals p

{- | Return all the declarations of static, global variables associated with the
peripherals of a program. -}
declarePeripherals :: Program -> [C.Definition]
declarePeripherals p = concatMap decls $ peripherals p
