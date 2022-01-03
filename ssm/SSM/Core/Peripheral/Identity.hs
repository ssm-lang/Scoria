{- | This module implements the core support for the identity peripheral, which is used
only to create references that can be used globally, outside the context of a process. -}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module SSM.Core.Peripheral.Identity
  ( Globals(..)
  , emptyGlobals
  ) where

import SSM.Core.Type
import SSM.Core.Ident
import SSM.Core.Reference
import SSM.Core.Backend
import SSM.Core.Peripheral

import qualified Data.Map as Map

-- | The @Globals@ datatype associates reference names with reference types
data Globals = Globals
  {
      references :: Map.Map Ident Type
  }
  deriving (Show, Eq)

-- | Empty @Globals@, containing no references
emptyGlobals :: Globals
emptyGlobals = Globals Map.empty

{- | A @Globals@ can be used regardless of the backend, since there is no associated
IO with a global reference. -}
instance Backend backend => IsPeripheral backend Globals where
    declareReference _ t id _ global =
        let m = references global
        in global { references = Map.insert id t m}
    
    declaredReferences _ globals =
        map (uncurry makeStaticRef) $ Map.toList $ references globals
    
    globalDeclarations p globals = []

    staticInitialization p globals = []
