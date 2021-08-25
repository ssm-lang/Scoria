{- | This module exposes the frontend language. For more documentation of the features
offered by the language, please refer to the two modules "SSM.Frontend.Box" and
"SSM.Frontend.Language". -}
module SSM.Language
    ( module SSM.Frontend.Language
    , module SSM.Frontend.Box
    , SSM
    , Compile

    ) where

import SSM.Frontend.Language
import SSM.Frontend.Box
import SSM.Frontend.Syntax
import SSM.Frontend.Compile
