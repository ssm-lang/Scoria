{- | Programs are parameterized over different backends. This file lists the available
backends and declares a typeclass with two associated types that are needed to generate
code in a backend-specific way. -}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module SSM.Core.Backend
  ( -- * Backend typeclass
    Backend(..)
    -- * Compiler-supported backends
  , C
  , C2
  , PrettyPrint
  , Interpret
  ) where

import qualified Language.C.Syntax as C


-- | Programs can be compiled to C
data C
data C2
-- | Programs can be pretty-printed
data PrettyPrint
-- | Programs can be interpreted
data Interpret

{- | Any type that implements the backend typeclass is available as a backend. The
associated types @Definition@ and @Statement@ mainly refers to how peripheral
initialization is handled. -}
class Backend backend where
  type Definition backend
  type Statement  backend
  type Expression backend

{- | For C, the definitions are of type @Definition@ from mainland-c, and the statements
are @BlockItem@s. -}
instance Backend C where
  type Definition C = C.Definition
  type Statement  C = C.BlockItem
  type Expression C = C.Exp

instance Backend C2 where
  type Definition C2 = C.Definition
  type Statement  C2 = C.BlockItem
  type Expression C2 = C.Exp

-- | The pretty-printing backend deals solely with strings
instance Backend PrettyPrint where
  type Definition PrettyPrint = String
  type Statement  PrettyPrint = String
  type Expression PrettyPrint = String

-- FIXME add meaning
instance Backend Interpret where
  type Definition Interpret = ()
  type Statement  Interpret = ()
  type Expression Interpret = ()
