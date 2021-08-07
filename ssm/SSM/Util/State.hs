{- | This module implements some machinery to generate fresh names. There's a typeclass
@IntState@ that specifies that a type has an @Int@ as part of its state. If something
has an @Int@ as part of its state, we can use that int to generate a fresh name given
that the @Int@ is used for only this. @MonadState s m@ is a little restricted in that
if we instantiate the @s@ to @Int@, the state can not contain more than that.

Example:

@
type Monad1 a = State (Int, Bool) a

instance IntState (Int, Bool) where
    getInt (i,_)   = i
    setInt i (_,b) = (i,b)
@

@
type Monad2 a = State Int a

instance IntState Int where
    getInt   = id
    setInt i = const i
@

@
data StateType = StateType { counter :: Int
                           , ...
                           , ...
                           }

type Monad3 a = State StateType a

instance IntState StateType where
    getInt      = counter
    setInt i st = st { counter = i }
@

The @fresh@ function can be used with all 3 of these monads, as the @IntState@ instance
tells the function how to retrieve and update the name-generating state from within the
more complex state.
-}
module SSM.Util.State
    ( IntState(..)
    , fresh
    ) where

import Control.Monad.State

{- | Class of types that has an @Int@ as part of them. Any State-monad states which
contains some sort of counter to generate fresh names should be given an instance of this
state. -}
class IntState a where
    {- | Get an integer from the state, equivalent to @gets counter@ if you have
    an explicit state with a record field called @counter@. -}
    getInt :: a -> Int
    -- | Update the @Int@-part of a state with a new @Int@
    setInt :: Int -> a -> a

-- | Trivial instance for states that consist of just a single @Int@
instance IntState Int where
    getInt     = id
    setInt i _ = i

-- | Generate fresh names from any monad whose state has an `IntState` instance
fresh :: (IntState s, MonadState s m) => m String
fresh = do
    s <- gets getInt
    modify $ \st -> setInt (s + 1) st
    return $ "fresh" ++ show s
