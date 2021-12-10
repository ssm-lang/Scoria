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
    , Handler(..)

    ) where

import           SSM.Core.Backend               ( Schedule )
import           SSM.Core.Ident                 ( Ident )
import           SSM.Core.Peripheral            ( Peripheral )
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
    = SSMProcedure Ident [Either SSMExp Reference]
    | OutputHandler (Handler backend)

data Handler backend = Handler
  { gen_handler    :: Int -> Int -> [Schedule backend]
  , pretty_handler :: String
  }

instance Show (QueueContent backend) where
  show (SSMProcedure id args) = "SSMProcedure " <> show id <> " " <> show args
  show (OutputHandler _) = "<output-handler>"

instance Eq (QueueContent backend) where
  SSMProcedure id1 args1 == SSMProcedure id2 args2 = id1 == id2 && args1 == args2
  OutputHandler _ == OutputHandler _ = undefined -- TODO

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
      -- | Peripherals
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
