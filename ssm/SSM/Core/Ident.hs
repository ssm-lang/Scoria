{- | Module that implements the identifiers used in the SSM compiler. -}
module SSM.Core.Ident
    ( Ident(..)
    , makeIdent
    , appendIdent
    ) where

-- * Identifiers

-- | Data type of Identifiers
data Ident = Ident
  { identName    :: String                -- ^ Identifiers has a name
  , identSrcInfo :: Maybe SrcInformation  -- ^ And possibly some source information
  }
  deriving (Show, Read)

makeIdent :: String -> Ident
makeIdent str = Ident str Nothing

appendIdent :: Ident -> Ident -> Ident
appendIdent (Ident str1 _) (Ident str2 _) = Ident (str1 <> str2) Nothing

{- | Equality of identifiers is done by comparing the `identName` component of
an identifier. The source information is not checked. -}
instance Eq Ident where
  Ident n _ == Ident m _ = n == m

{- | Ordering of identifiers is done by checking the ordering of the `identName`
component of an identifier. The source information is not checked. -}
instance Ord Ident where
  Ident n _ <= Ident m _ = n <= m

-- | Source information (File, Line, Column)
type SrcInformation = (String, Int, Int)
