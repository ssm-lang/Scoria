module SSM.Util.HughesList where

-- | Using Writer monad with [] recursively (as we do here) produce absolutely
-- mad complexities, so we use a hugheslist and turn concatenation into
-- composition instead.
type Hughes a = [a] -> [a]

toHughes :: [a] -> Hughes a
toHughes xs = (xs ++)

fromHughes :: Hughes a -> [a]
fromHughes = ($[])

snoc :: Hughes a -> a -> Hughes a
snoc hl a = hl <> toHughes [a]

emptyHughes :: Hughes a
emptyHughes = toHughes []