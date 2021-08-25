module SSM.Core.Ident where

-- Identifiers

-- | Data type of Identifiers
data Ident = Ident { identName :: String, identSrcInfo :: Maybe SrcInformation}
  deriving (Show, Read)

instance Eq Ident where
  Ident n _ == Ident m _ = n == m

instance Ord Ident where
  Ident n _ <= Ident m _ = n <= m

-- | Source information (File, Line, Column)
type SrcInformation = (String, Int, Int)
