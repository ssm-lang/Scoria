{-| This module implements Hughes Lists, which is a kind of list where
concatenation is turned into composition, which guarantees linear time.
Koen told me about them, and I can only guess that they are named after
John.

Using something like a @Writer [Int]@ means that you need to emit output
by e.g @tell [5]@. If the function that emits this output is recursive this
will produce something like @((((([5] ++ [6]) ++ [7]) ++ [8]) ++ [9]) ++ ...@,
which as a result of how @++@ is implemented has complexity @O(n^3)@.

If we can turn this appending into composition instead, we can gain the linear
complexity we desire.

@
data Hughes a = [a] -> [a]

toHughes xs = (xs ++)

fromHughes = ($[]) -- apply Hughes List to empty list

h1 ++ h2 = h1 . h2
@

Now imagine @h1 = ([1,2] ++)@ and @h2 = ([3,4] ++)@, then @fromHughes (h1 ++ h2)@ is

@
fromHughes (([1,2] ++) . ([3,4] ++))           ==
fromHughes (\\x -> (([1,2] ++) . ([3,4] ++)) x) ==
(\\x -> (([1,2] ++) . ([3,4] ++)) x) $ []       ==
(([1,2] ++) .  ([3,4] ++)) []                  ==
([1,2] ++) ([3,4] ++ [])                       ==
@

Now, reading from this list forces all of the list to be evaluated, since composition is
evaluated by applying to argument to the rightmost function and piping the result through
all the composed functions. Since the list is now constructed from the 'back', it is
traversed only once. Linear Time.

@
[1,2] ++ [3,4]                                 ==
[1,2,3,4]
@

-}
module SSM.Util.HughesList
    ( -- * Hughes Lists
      -- ** Hughes List
      Hughes

      -- ** Construction
    , toHughes
    , emptyHughes

      -- ** Destruction
    , fromHughes

      -- ** Operations
    , (SSM.Util.HughesList.++)
    , snoc
    ) where

-- | Hughes lists
type Hughes a = [a] -> [a]

-- | Turn an ordinary Haskell List into a Hughes List.
toHughes :: [a] -> Hughes a
toHughes xs = (xs Prelude.++)

-- | Turn a Hughes List into an ordinary Haskell List.
fromHughes :: Hughes a -> [a]
fromHughes = ($[])

-- | Append an element to the end of a Hughes List.
snoc :: Hughes a -> a -> Hughes a
snoc hl a = hl <> toHughes [a]

-- | Appending two hughes list is done by using the monoid instance for functions.
(++) :: Hughes a -> Hughes a -> Hughes a
(++) = (<>)

-- | An empty Hughes List.
emptyHughes :: Hughes a
emptyHughes = toHughes []