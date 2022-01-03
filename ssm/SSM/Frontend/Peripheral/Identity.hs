{- | This module implements an identity peripheral. The identity peripheral has
no IO actions associated with it, and is used solely to create references that
exist in the global scope.  -}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module SSM.Frontend.Peripheral.Identity
  ( global
  , Globals(..) -- only exposed for testing, FIXME
  ) where

import SSM.Core hiding (peripherals)
import SSM.Core.Peripheral.Identity

import SSM.Util.State

import SSM.Frontend.Compile
import SSM.Frontend.Ref

import Data.Proxy
import qualified Data.Map as Map

import Control.Monad.State

{- | Create a global reference. The reference is created in the compile monad and
can be shared across the Scoria program with the @ImplicitParams@ extension.

@
program :: Compile backend ()
program = do
    ref <- global @Word8
    let ?ref = ref

    schedule main

main :: (?ref :: Ref Word8) => SSM ()
main = assign ?ref 5
@

-}
global :: forall a backend . (Backend backend, SSMType a) => Compile backend (Ref a)
global = do
    n <- fresh
    let id = Ident ("global" <> show n) Nothing
    let t = mkReference $ typeOf $ Proxy @a

    st <- get
    let maybeg = Map.lookup "globals" (peripherals st)
        emptyg = Peripheral @backend emptyGlobals
        m      = maybe emptyg (\x -> x) maybeg
        m'     = declareReference (Proxy @backend) typ id 0 m

    put $ st { peripherals = Map.insert "globals" m' (peripherals st) }

    let ref = makeStaticRef id typ
    return (Ptr ref)
  where
      typ :: Type
      typ = mkReference $ typeOf $ Proxy @a
