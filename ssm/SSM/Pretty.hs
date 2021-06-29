module SSM.Pretty
    ( -- * Prettyprint SSM programs
      prettySSM
      -- * Prettyprint low level programs
    , prettyProgram
    ) where

--import SSM.Core.Syntax ( SSM )
import SSM.Frontend.Syntax ( SSM, transpile )
import SSM.Pretty.Syntax ( prettyProgram )

prettySSM :: SSM () -> String
prettySSM = prettyProgram . transpile