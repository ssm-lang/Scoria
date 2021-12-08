{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SSM.Frontend.Peripheral.Identity where

import           SSM.Core.Ident
import           SSM.Core.Peripheral.Identity
import           SSM.Core.Reference hiding (Ref)
import           SSM.Core.Type

import           SSM.Util.State

import           SSM.Frontend.Compile
import           SSM.Frontend.Exp
import           SSM.Frontend.Ref

import           Data.Proxy

import           Control.Monad.State

-- | Generate a global SV
global :: forall backend a . SSMType a => Compile backend (Ref a)
global = do
    n <- fresh
    let id = Ident n Nothing
    let t  = mkReference $ typeOf $ Proxy @a
    -- modify $ \st ->
    --     st { generatedGlobals = addIdentitySV id t $ generatedGlobals st }
    return $ Ptr $ makeStaticRef id t
