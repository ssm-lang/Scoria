module SSM.Pretty
    ( -- * Prettyprint SSM programs
      prettySSM
      -- * Prettyprint low level programs
    , prettyProgram
    ) where

import SSM.Core.Syntax ( SSM )
import SSM.Core.LowSyntax ( transpile )
import SSM.Pretty.LowSyntax ( prettyProgram )

prettySSM :: SSM () -> String
prettySSM = prettyProgram . transpile