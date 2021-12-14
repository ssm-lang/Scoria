{- | Programs are parameterized over different backends. This file lists the available
backends and declares some type families that can be used to talk about backend-specific
code in a general way. -}
{-# LANGUAGE TypeFamilies #-}
module SSM.Core.Backend
  ( C
  , PrettyPrint
  , Interpret
  , Definition
  , Statement
  ) where

import qualified Language.C.Syntax as C

-- | Programs can be compiled to C
data C
data PrettyPrint
data Interpret

-- | Type of top-level declarations
type family Definition backend where
    Definition C           = C.Definition
    Definition PrettyPrint = String
    Definition Interpret   = () -- FIXME add meaning

-- | Type of statements
type family Statement backend where
    Statement C           = C.BlockItem
    Statement PrettyPrint = String
    Statement Interpret   = () -- FIXME add meaning
