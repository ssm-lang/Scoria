{- | This module implements an identity peripheral. This is a peripheral that has no side
effects. It is suitable for declaring references that should exist in the global scope
rather than in the context of an activation record. -}
module SSM.Core.Peripheral.Identity where

import           SSM.Core.Ident
import           SSM.Core.Peripheral
import           SSM.Core.Reference
import           SSM.Core.Type

import qualified Data.Map                      as Map

modulename :: String
modulename = "SSM.Core.Peripheral.Identity"

data IdentityPeripheral = IdentityPeripheral
    { identitySVs :: (Map.Map Ident Type)
    }
  deriving (Show, Read, Eq)

instance IsPeripheral IdentityPeripheral where
    declaredReferences ip =
        map (uncurry makeStaticRef) $ Map.toList $ identitySVs ip
    mainInitializers ip =
        map (Normal . uncurry makeStaticRef) $ Map.toList $ identitySVs ip

emptyIdentityPeripheral :: IdentityPeripheral
emptyIdentityPeripheral = IdentityPeripheral Map.empty

getIdentitySVs :: IdentityPeripheral -> [(Ident, Type)]
getIdentitySVs = Map.toList . identitySVs

addIdentitySV :: Ident -> Type -> IdentityPeripheral -> IdentityPeripheral
addIdentitySV id t ip = if Map.member id $ identitySVs ip
    then error $ concat
        [ modulename
        , ".addIdentitySV error ---\n"
        , "reference name "
        , identName id
        , "already registered"
        ]
    else ip { identitySVs = Map.insert id t $ identitySVs ip }
