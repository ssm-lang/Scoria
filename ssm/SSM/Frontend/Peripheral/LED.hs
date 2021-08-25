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

type LED = Bool

on :: Exp LED
on = true'

off :: Exp LED
off = false'

onoffLED :: Int -> Compile (Ref LED)
onoffLED i = do
    n <- fresh
    let id = Ident n Nothing
    modify $ \st -> st { ledperipherals = addOnOffLED i id $ ledperipherals st }
    return $ Ptr $ makeStaticRef id (Ref TBool)
