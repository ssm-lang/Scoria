{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
module SSM.Frontend.Peripheral.Identity where

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

instance IsPeripheral C Globals where
    declareReference _ t id _ global =
        let m = references global
        in global { references = Map.insert id t m}
    
    declaredReferences _ globals =
        map (uncurry makeStaticRef) $ Map.toList $ references globals
    
    globalDeclarations p globals =
        flip map (declaredReferences p globals) $ \ref ->
            [cedecl| $ty:(svt_ $ dereference $ refType ref) $id:(refName ref); |]

    staticInitialization p globals = flip concatMap (declaredReferences p globals) $ \ref ->
        let bt = dereference $ refType ref
            init = initialize_ bt [cexp| &$id:(refName ref) |]
            assign = assign_ bt [cexp| &$id:(refName ref)|] [cexp|0|] [cexp|0|]
        in [citems| $exp:init; $exp:assign; |]

global :: forall backend a . (IsPeripheral backend Globals, SSMType a) => Compile backend (Ref a)
global = do
    n <- fresh
    let id = Ident n Nothing
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
