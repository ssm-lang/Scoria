{- | This module provides a machinery that helps with creating and applying procedures.

If we write a procedure like this:

@
-- wait for t units of time
delay :: Exp Word64 -> SSM ()
delay t = do
  v <- var true'
  after t v false'
  wait [v]
@

the "SSM.Frontend.Syntax" representation of this is

@
[ NewRef (Captured "v") (Lit TBool (LBool True))
, After (Var TUInt64 "t") ("v", Ref TBool) (Lit TBool (LBool False))
, Wait [("v", Ref TBool)]
]
@

There are a few issues with this representation. First of all, in this expression
@t@ appears as a magic variable. It is not clear where @t@ is bound. Secondly, if we
now imagine that another process forks this procedure

@
fork [ delay 50 ]
@

this would become

@
[ Fork ssm ]
@

where if you evaluate the @ssm@ computation you would get back

@
[ NewRef (Captured "v") (Lit TBool (LBool True))
, After (Var TUInt64 "t") ("v", Ref TBool) (Lit TBool (LBool False))
, Wait [("v", Ref TBool)]
]
@

What are we actually forking here? Are we forking a procedure or are we forking
a list of statements? If it's a procedure, nothing at all in this definition tells us
what name that procedure has so that we can generate code for it. If it's a list of
statements, that's an issue because the runtime system does not support forking code
like this. It only supports forking procedures.

If @delay@ was instead declared like this

@
-- wait for t units of time
delay :: Exp Word64 -> SSM ()
delay = box "delay" ["t"] $ \t -> do
  v <- var true'
  after t v false'
  wait [v]
@

applying delay to an argument and evaluating the resulting expression now yields
the statements

@
[ Procedure "delay"
, Argument "delay" "t" -value that delay was applied to-
, NewRef (Captured "v") (Lit TBool (LBool True))
, After (Var TUInt64 "t") ("v", Ref TBool) (Lit TBool (LBool False))
, Wait [("v", Ref TBool)]
, Result "delay"
]
@

In this sequence of statements it becomes clear where the procedure @delay@ begins, where
it ends, and where its parameters are bound. When we are transpiling this high level
representation of a program we can inspect the prefix of this list that specifies the
procedure name, and check if we've seen this procedure before. If we have not, we will
generate low level representation of it. If we have, we do nothing. We actually do
something, however. We will take the prefix of the list (`SSM.Frontend.Syntax.Procedure`
& `SSM.Frontend.Syntax.Argument` constructors), and turn that into a
@(String, [Either SSMExp Reference])@ that describes the call instead.

Now, while it is quite straight forward to write the `box` stuff, it does clutter the eyes
a bit. I am thinking that we can write a GHC plugin which annotates definitions with the
source information. The definition

@
-- wait for t units of time
delay :: Exp Word64 -> SSM ()
delay t = do
  v <- var true'
  after t v false'
  wait [v]
@

Would be annotated by the plugin with

@
-- wait for t units of time
delay :: Exp Word64 -> SSM ()
delay t = (do
  v <- var true'
  after t v false'
  wait [v]) \`annotateDef\` ("delay", ["t"], -source location-)
@
-}
{-# LANGUAGE DeriveDataTypeable #-}
module SSM.Frontend.Box
    ( -- * Box machinery

      -- ** Procedures
      Box(..)
    , BoxNullary(..)

      -- ** Arguments
    , Arg(..)

      -- ** Results
    , Res(..)
    , ROUTINE(ROUTINE)
    , routine
    ) where

import Data.Generics (Data)
import SSM.Frontend.Syntax ( SSM, SSMStm(Procedure, Result), emit, Ident(..) )

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
    result name () = emit $ Result (Ident name Nothing)

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

-- | There is a dummy `Arg` instance for @()@, which we can use to piggyback
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
        emit $ Procedure (Ident name Nothing)
        (x',_) <- arg name xs x
        y'     <- f x'
        result name y'

-- | A tag constructor that can be used with annotation pragmas
data ROUTINE = ROUTINE
  deriving Data

routine :: a -> a
routine _ = error $ unlines
  [ "routine error ---"
  , "  must enable SSM.Plugin with routine mode for routine to work"
  , "  paste this at the top of your program"
  , "{-# OPTIONS_GHC -fplugin=SSM.Plugin -fplugin-opt=SSM.Plugin:mode=routine #-}"
  ]
