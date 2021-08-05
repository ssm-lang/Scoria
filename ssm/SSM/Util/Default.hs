module SSM.Util.Default where

-- | Typeclass that allows us to attach default values to certain types.
class Default a where
  def :: a
