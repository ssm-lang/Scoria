module SSM.Pretty
    ( -- * Prettyprint SSM programs
      prettySSM
      -- * Prettyprint low level programs
    , prettyProgram
    ) where

--import SSM.Core.Syntax ( SSM )
import SSM.Core.Syntax
import SSM.Pretty.Syntax ( prettyProgram )

prettySSM :: SSMProgram a => a -> String
prettySSM = prettyProgram . toProgram
