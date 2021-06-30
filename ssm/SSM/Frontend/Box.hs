-- | This module provides a machinery that helps with creating and applying procedures.
module SSM.Frontend.Box
    ( -- * Box machinery

      -- ** Procedures
      Box(..)
    , BoxNullary(..)

      -- ** Arguments
    , Arg(..)

      -- ** Results
    , Res(..)
    ) where

import SSM.Frontend.Syntax ( SSM, SSMStm(Procedure, Result), emit )

-- | The class of types that can appear as arguments to our procedures
class Arg a where
    arg :: String             -- ^ Name of the procedure the argument is applied to
        -> [String]           -- ^ List of parameter names (head is the next name)
        -> a                  -- ^ Argument itself
        -> SSM (a, [String])  -- ^ Return the argument and the rest of the names

-- | If we can apply both a's and b's to our program, we can apply pairs of a's and b's
instance (Arg a, Arg b) => Arg (a,b) where
    arg name names (x,y) = do
        (x', names')  <- arg name names  x
        (y', names'') <- arg name names' y
        return ((x',y'), names'')

-- | Class of types that can be the result of our procedures
class Res b where
    result :: String  -- ^ Name of the procedure we are returning from
           -> b       -- ^ The result
           -> SSM b

-- | Possible results of SSM procedures (they can't return anything)
instance Res () where
    result name () = emit $ Result name

-- | Class of types that can be boxed (turned into a procedure)
class Box b where
    box :: Arg a     -- If a is something we can apply procedures to
        => String    -- ^ Name of the procedure
        -> [String]  -- ^ Parameters names of the procedure
        -> (a -> b)  -- ^ Procedure body
        -> (a -> b)

-- | Class of types that can be boxed, but which does not take any parameters.
class BoxNullary b where
  boxNullary :: String -> b -> b


-- Dummy instances

-- | There is a dummy `Arg` instance for `()`, which we can use to piggyback
-- on the box machinery that's already in place.
instance Res b => BoxNullary (SSM b) where
  boxNullary name b = box name [] (\() -> b) $ ()

instance Arg () where
  arg name names () = return ((), names)

instance (Arg b, Box c) => Box (b -> c) where
    box name xs f = curry (box name xs (uncurry f))

{- | This instance is what we'll use the most. It takes a SSM procedure, which boils down
to a list of statements, and adds the Procedure constructor and argument constructors to
the head of the list, and inserts a Result constructor at the end. This makes it
unambiguous where a procedure begins and where it ends. -}
instance Res b => Box (SSM b) where
    box name xs f = \x -> do
        emit $ Procedure name
        (x',_) <- arg name xs x
        y'     <- f x'
        result name y'
