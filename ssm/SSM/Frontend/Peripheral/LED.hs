{- | This module implements support for controlled LEDs from the EDSL. A LED has a binary
state that is either `on` or `off`. As with any normal SSM references, LEDs can e.g be
scheduled to turn on at a specific time

@
after (msec 1000) led on
@

and they can be waited on

@
wait [led]
@

Internally the state of a LED is represented with a @Bool@, but it is advisable to talk
about them by using the type synonym `LED` and the two values `on` and `off`,
representing the two different states a LED can have.
-}
module SSM.Frontend.Peripheral.LED where

import           SSM.Core.Ident
import           SSM.Core.Peripheral.LED
import           SSM.Core.Syntax         hiding ( ledperipherals )

import           SSM.Frontend.Compile
import           SSM.Frontend.Exp
import           SSM.Frontend.Language
import           SSM.Frontend.Ref

import           SSM.Util.State

import           Control.Monad.State
import           Data.Word

{- | On-off LEDs can be either on or off, so their state is semantically equivalent to
a boolean state. -}
type LED = Bool

-- | A value representing the LED-state on
on :: Exp LED
on = true'

-- | A value representing the LED-state off
off :: Exp LED
off = false'

{- | Statically create and initialize a binary stated LED, identified by a single
integer. The meaning of this integer is not well defined yet, and it is assumed that a
meaning exists in the runtime. -}
onoffLED :: Word8 -> Compile (Ref LED, Handler)
onoffLED i = do
    n <- fresh
    let id = Ident n Nothing
    modify $ \st -> st { ledperipherals = addOnOffLED i id $ ledperipherals st }
    let ref     = makeStaticRef id (Ref TBool)
    let handler = StaticOutputHandler ref i
    return $ (Ptr ref, handler)
