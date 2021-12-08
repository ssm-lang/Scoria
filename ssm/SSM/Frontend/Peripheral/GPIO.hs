module SSM.Frontend.Peripheral.GPIO where

import           SSM.Core.Peripheral.GPIO
import           SSM.Core.Syntax         hiding ( gpioperipherals )
import           SSM.Frontend.Compile
import           SSM.Frontend.Exp
import           SSM.Frontend.Language
import           SSM.Frontend.Ref
import           SSM.Frontend.Syntax

import           Control.Monad.State

import           Data.Word

{- | A switch can provide input that is either high or low, so we use a @Bool@ to model
this binary state. It would perhaps improve readabilty to change this to a dedicated
datatype with states @HIGH@ and @LOW@ later on down the road when we have support for
user defined ADTs. -}
type SW = Bool

-- | Is a @Ref SW@ high?
isHigh :: Ref SW -> Exp Bool
isHigh = deref

-- | Is a @Ref SW@ low?
isLow :: Ref SW -> Exp Bool
isLow = not' . isHigh

-- | Create a @Ref SW@ by identifying a GPIO pin with a unique ID. E.g GPIO 1.
switch :: Word8 -> Compile backend (Ref SW)
switch i = do
    n <- fresh
    let id = Ident n Nothing
    -- modify $ \st ->
    --     st { gpioperipherals = addSwitchGPIO i id (gpioperipherals st) }
    return $ Ptr $ makeStaticRef id (Ref TBool)
