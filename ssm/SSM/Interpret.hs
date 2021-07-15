{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module SSM.Interpret
    ( interpret
    , Interpretable
    , customContQueueSize
    , customEventQueueSize
    , customQueueSizes
    ) where

import SSM.Core.Syntax
import qualified SSM.Interpret.Interpreter as I
import SSM.Interpret.Trace (Trace)

-- | Interpret an SSM program
interpret :: Interpretable a => a -> Trace
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
return them as they are given.

Note: Slightly unsure of the use of overlapping here - I will need to read into
that and document it more precisely. -}
instance {-# OVERLAPPING #-} Interpretable I.InterpretConfig where
    toConfiguration = id

-- | Specify custmo continuation queue & event queue-sizes
customQueueSizes :: Interpretable a => Int -> Int -> a -> I.InterpretConfig
customQueueSizes cont event a = customContQueueSize cont (customEventQueueSize event a)

-- | Specify custom continuation queue size
customContQueueSize :: Interpretable a => Int -> a -> I.InterpretConfig
customContQueueSize i a = (toConfiguration a) { I.boundContQueueSize = i }

-- | Specify custom event queue size
customEventQueueSize :: Interpretable a => Int -> a -> I.InterpretConfig
customEventQueueSize i a = (toConfiguration a) {I.boundEventQueueSize = i }
