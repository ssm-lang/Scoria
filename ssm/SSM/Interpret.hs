{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module SSM.Interpret where

import SSM.Core.Syntax
import qualified SSM.Interpret.Interpreter as I
import SSM.Interpret.Trace

-- | Interpret an SSM program
interpret :: Interpretable a => a -> Output
interpret = I.interpret . toConfiguration


-- instead of just having interpret :: SSMProgram a => a -> Output` like we do now,
-- we can do this

-- | Class of types that can be interpreted
class Interpretable a where
    -- | Turn an element of this type into an interpretation configuration
    toConfiguration :: a -> I.InterpretConfig

-- | Programs can be interpreter with default values
instance SSMProgram a => Interpretable a where
    toConfiguration a = I.InterpretConfig 1024 2048 (toProgram a)

{- | Dummy instance for custom configurations. Don't do anything with them, just
return them as they are given. -}
instance Interpretable I.InterpretConfig where
    toConfiguration = id

{- | To run a test with custom sizes, use this function to override the default
values. -}
withCustomSizes :: Int -> Int -> Program -> I.InterpretConfig
withCustomSizes = I.InterpretConfig
