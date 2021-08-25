{- | This module defines a type class @CPeripheral@ that can be used to generate C code
for a IO peripheral. Every peripheral should be made an instance of this type class to
facilitate easier code generation. -}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
module SSM.Backend.C.Peripheral
    ( initPeripherals
    , declarePeripherals
    ) where

import           SSM.Core.Ident
import           SSM.Core.Peripheral.GPIO
import           SSM.Core.Peripheral.LED
import           SSM.Core.Syntax

import           SSM.Backend.C.Identifiers

import           Language.C.Quote.GCC
import qualified Language.C.Syntax             as C

import           Data.Maybe

-- | Class of types that represent IO peripherals
class CPeripheral a where
    {- | Fetch the C declarations that represent the variables bound by the IO
    peripheral. These declarations end up in the global scope of the generated C
    program. -}
    decls    :: a -> [C.Definition]

    {- | @maininit@ returns a list of statements that should be executed before the
    program is allowed to run. These statements are typically used to initialize the IO
    behaviour or a peripheral. -}
    maininit :: a -> [C.BlockItem]

{- | Item to be placed in a HList, where the only thing we know about the internal object
is that it has an instance of `CPeripheral`. -}
data CPeripherable where
    Peripheral ::CPeripheral a => a -> CPeripherable

-- | Dummy instance that propagates the `CPeripheral` instance to the internal peripheral
instance CPeripheral CPeripherable where
    decls (Peripheral a) = decls a
    maininit (Peripheral a) = maininit a

-- | Fetch all peripherals from a program, and hide them within a list of `CPeripherable`
allPeripherals :: Program -> [CPeripherable]
allPeripherals p = map fromJust $ filter isJust $ [gpiop, ledp]
  where
      gpiop = fmap Peripheral $ gpioperipherals p
      ledp  = fmap Peripheral $ ledperipherals p

-- | Return all the statements that initialize the peripherals statically
initPeripherals :: Program -> [C.BlockItem]
initPeripherals p = concatMap maininit $ allPeripherals p

{- | Return all the declarations of static, global variables associated with the
peripherals of a program. -}
declarePeripherals :: Program -> [C.Definition]
declarePeripherals p = concatMap decls $ allPeripherals p

-- | Generate C code for GPIO peripherals. Only switches supported right now
instance CPeripheral GPIOPeripheral where
    decls gp = switches -- ++ dacs ++ adcs
      where
        switches :: [C.Definition]
        switches =
            [cedecl| $esc:("// switch GPIOs") |] : map switch (switchpins gp)

        switch :: (Int, Ident) -> C.Definition
        switch (i, id) = [cedecl| $ty:(svt_ TBool) $id:(identName id); |]

    maininit gp = initswitches
      where
        initswitches :: [C.BlockItem]
        initswitches = map initswitch (switchpins gp)

        initswitch :: (Int, Ident) -> C.BlockItem
        initswitch (i, id)
            = [citem| $id:initialize_static_input_device(&$id:(identName id), $int:i); |]

-- | Generate C code for LED peripherals. Only on-off LEDs supported right now
instance CPeripheral LEDPeripheral where
    decls lp = onoffleds -- ++ dimmableleds
      where
        onoffleds :: [C.Definition]
        onoffleds = [cedecl| $esc:("// LEDs") |] : map onoffled (onoffLEDs lp)

        onoffled :: (Int, Ident) -> C.Definition
        onoffled (i, id) = [cedecl| $ty:(svt_ TBool) $id:(identName id); |]

    maininit lp = initonoffleds -- ++ initdimmableleds
      where
        initonoffleds :: [C.BlockItem]
        initonoffleds = map initonoffled $ onoffLEDs lp

        initonoffled :: (Int, Ident) -> C.BlockItem
        initonoffled (i, id) = [citem| $id:fork( $id:top_return
                                               , SSM_ROOT_PRIORITY + (1 << SSM_ROOT_DEPTH)
                                               , SSM_ROOT_DEPTH - 1
                                               , &$id:(identName id)
                                               , $int:i
                                               ); |]
