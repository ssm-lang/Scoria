{- | This module defines a type class @CPeripheral@ that can be used to generate C code
for a IO peripheral. Every peripheral should be made an instance of this type class to
facilitate easier code generation. -}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
module SSM.Backend.C.Peripheral where

import           SSM.Core

import           SSM.Backend.C.Identifiers
import           SSM.Backend.C.Types

import           Language.C.Quote.GCC
import qualified Language.C.Syntax             as C

{- | Get the global declarations made by a peripheral. Usually this will just be global
references. -}
decls :: Peripheral C -> [C.Definition]
decls = undefined
-- decls (Peripheral a) = map declSingle $ declaredReferences a
--  where
--   declSingle :: Reference -> C.Definition
--   declSingle r =
--     [cedecl| $ty:(svt_ $ dereference $ refType r) $id:(refName r);|]

{- | Get the statements that initializes this peripheral. These statements should be
executed in the program initialization point, before the program actually runs. -}
maininit :: Peripheral C -> [C.BlockItem]
maininit = undefined
-- maininit (Peripheral a) = concatMap compInitializer $ mainInitializers a
--  where
--   compInitializer :: Initializer -> [C.BlockItem]
--   compInitializer i = case i of
--     Independent ind -> case ind of
--       BLEEnable -> [[citem| enable_ble_stack(); |]]
--     Normal ref ->
--       let bt     = dereference $ refType ref
--           init   = initialize_ bt [cexp|&$id:(refName ref)|]
--           assign = assign_ bt [cexp|&$id:(refName ref)|] [cexp|0|] [cexp|0|]
--       in  [citems| $exp:init; $exp:assign; |]
--     StaticInput si ref -> case si of
--       Switch id ->
--         [ [citem| $id:initialize_static_input_device((typename ssm_sv_t *) &$id:(refName ref).sv, $int:id);|]
--         ]
--       BLEScan ->
--         [ [citem| $id:initialize_static_input_ble_scan_device(&$id:(refName ref).sv);|]
--         ]

-- | Return all the statements that initialize the peripherals statically
initPeripherals :: Program C -> [C.BlockItem]
initPeripherals p = concatMap maininit $ peripherals p

{- | Return all the declarations of static, global variables associated with the
peripherals of a program. -}
declarePeripherals :: Program C -> [C.Definition]
declarePeripherals p = concatMap decls $ peripherals p
