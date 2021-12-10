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
module SSM.Frontend.Peripheral.Identity ( global ) where

import SSM.Core hiding (peripherals)

import SSM.Util.State

import SSM.Frontend.Compile
import SSM.Frontend.Ref

import SSM.Backend.C.Identifiers
import SSM.Backend.C.Types

import Data.Proxy
import qualified Data.Map as Map

import Control.Monad.State

import           Language.C.Quote.GCC
import qualified Language.C.Syntax as C

data Globals = Globals { references :: Map.Map Ident Type }
  deriving (Show, Eq)

emptyGlobals :: Globals
emptyGlobals = Globals Map.empty

-- | The identity peripheral works regardless of backend, since no IO is involved
instance IsPeripheral backend Globals where
    declareReference _ t id _ global =
        let m = references global
        in global { references = Map.insert id t m}
    
    declaredReferences _ globals =
        map (uncurry makeStaticRef) $ Map.toList $ references globals
    
    globalDeclarations p globals = []

    staticInitialization p globals = []

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
global :: forall a backend . SSMType a => Compile backend (Ref a)
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
