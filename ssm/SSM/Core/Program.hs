{- | This module implements the `Procedure` type and the `Program` type, which represents
the kind of procedures we can have in an SSM program and how an entire SSM program is
represented. -}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
module SSM.Core.Program
    ( Procedure(..)
    , QueueContent(..)
    , entry
    , Program(..)
    , SSMProgram(..)

    ) where

import           SSM.Core.Backend               ( Schedule )
import           SSM.Core.Ident                 ( Ident )
import           SSM.Core.Peripheral            ( Peripheral, Handler )
import           SSM.Core.Reference             ( Reference )
import           SSM.Core.Syntax                ( SSMExp
                                                , Stm
                                                )
import           SSM.Core.Type                  ( Type )

import           Data.Word                      ( Word8 )

import qualified Data.Map                      as Map

-- | An SSM procedure
data Procedure = Procedure
    { name      :: Ident            -- ^ Name of the procedure.
    , arguments :: [(Ident, Type)]  -- ^ Parameter names and types of the procedure.
    , body      :: [Stm]            -- ^ Statements that make up this procedure.
    }
    deriving (Eq, Show)

-- | A @QueueContent@ is something that can be scheduled when a program begins executing.
data QueueContent backend
    {- | SSM procedures can be scheduled initially. Right now it is assumed that only
    one SSM procedure will ever be scheduled initiailly, and that it will have no
    arguments. The constructor looks like this, however, in preparation for any future
    changes we might want to make. I might remove this second argument... -}
    = SSMProcedure Ident [Either SSMExp Reference]
--    | Handler Handler  -- ^ Handlers can be scheduled
    | Handler (Int -> Int -> Schedule backend)

instance Show (QueueContent backend) where
  show (SSMProcedure id args) = "SSMProcedure " <> show id <> " " <> show args
  show (Handler _) = "<output-handler>"

instance Eq (QueueContent backend) where
  SSMProcedure id1 args1 == SSMProcedure id2 args2 = id1 == id2 && args1 == args2
  Handler _ == Handler _ = undefined -- TODO

{- | Get the identifier of the SSM procedure that is scheduled at the start of a SSM
program -}
entry :: Program backend -> Ident
entry p = getInitialProcedure' $ initialQueueContent p
  where
    getInitialProcedure' :: [QueueContent backend] -> Ident
    getInitialProcedure' [] = error $ concat
        [ "SSM.Core.Syntax.getInitialProcedure error ---\n"
        , "no initial SSM procedure set to be scheduled when "
        , "running the program"
        ]
    getInitialProcedure' (SSMProcedure id _ : _ ) = id
    getInitialProcedure' (_                 : xs) = getInitialProcedure' xs

-- | Program definition
data Program backend = Program
    { -- | The things that should be scheduled when the program starts
      initialQueueContent :: [QueueContent backend]
      -- | Map that associates procedure names with their definitions.
    , funs                :: Map.Map Ident Procedure
      -- | Name and type of references that exist in the global scope.
      -- | Any peripherals used by the program
    , peripherals         :: [Peripheral backend]
    }
    deriving (Show)

instance Eq (Program backend) where
  p1 == p2 = initialQueueContent p1 == initialQueueContent p2 &&
             funs p1 == funs p2

-- | Class of types that can be converted to a `Program`.
class SSMProgram backend a where
  -- | This function takes an @a@ and converts it to a `Program`
  toProgram :: a -> Program backend

-- | Dummy instance for `Program`. Does nothing -- defined to be the identity function.
instance SSMProgram backend (Program backend) where
    toProgram = id
