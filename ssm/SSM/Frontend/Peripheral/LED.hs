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

import           SSM.Core.Ident                 ( Ident(Ident) )
import           SSM.Core.Peripheral.LED        ( addOnOffLED )
import           SSM.Core.Peripheral
import           SSM.Core.Reference             ( makeStaticRef )
import           SSM.Core.Type                  ( Type(TBool)
                                                , mkReference
                                                )

import           SSM.Frontend.Compile           ( Compile
                                                , CompileSt(ledperipherals)
                                                )
import           SSM.Frontend.Exp               ( Exp )
import           SSM.Frontend.Language          ( (==.)
                                                , Ref
                                                , deref
                                                , false
                                                , not'
                                                , true
                                                )
import           SSM.Frontend.Ref               ( Ref(Ptr) )
import           SSM.Frontend.Syntax            ( SSM
                                                , emit
                                                , SSMStm(Handler)
                                                )

import           SSM.Util.State                 ( fresh )

import           Control.Monad.State            ( modify )
import           Data.Word                      ( Word8 )

{- | On-off LEDs can be either on or off, so their state is semantically equivalent to
a boolean state. -}
type LED = Bool

-- | A value representing the LED-state on
on :: Exp LED
on = true

-- | A value representing the LED-state off
off :: Exp LED
off = false

-- | Is a LED on?
isON :: Ref LED -> Exp Bool
isON = (==.) on . deref

-- | Is a LED off?
isOFF :: Ref LED -> Exp Bool
isOFF = not' . isON

{- | Statically create and initialize a binary stated LED, identified by a single
integer. The meaning of this integer is not well defined yet, and it is assumed that a
meaning exists in the runtime. This function also returns a handler that will actually
perform the IO side-effects. This must be scheduled to run, or else it will not
perform any side effects. -}
onoffLED :: Word8 -> Compile (Ref LED, SSM ())
onoffLED i = do
    -- generate fresh name for reference
    n <- fresh
    let id = Ident n Nothing

    -- modify internal LED object to know about this reference
    modify $ \st -> st { ledperipherals = addOnOffLED i id $ ledperipherals st }

    -- create the reference to return to the developer
    let ref     = makeStaticRef id (mkReference TBool)

    -- create the SSM handler to return to the developer
    let handler = emit $ Handler $ Output (LED i) ref

    -- return the reference and the SSM () that performs the actual IO
    return $ (Ptr ref, handler)
