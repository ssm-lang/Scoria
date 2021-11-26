{- | This module exposes all the internals of the core directory. -}
module SSM.Core
    ( module SSM.Core.Ident
    , module SSM.Core.Peripheral
    , module SSM.Core.Peripheral.Identity
    , module SSM.Core.Peripheral.GPIO
    , module SSM.Core.Peripheral.LED
    , module SSM.Core.Peripheral.BasicBLE
    , module SSM.Core.Program
    , module SSM.Core.Reference
    , module SSM.Core.Syntax
    , module SSM.Core.Type
    , module SSM.Core.Backend
    ) where

import           SSM.Core.Ident
import           SSM.Core.Peripheral
import           SSM.Core.Peripheral.GPIO
import           SSM.Core.Peripheral.Identity
import           SSM.Core.Peripheral.LED
import           SSM.Core.Peripheral.BasicBLE
import           SSM.Core.Program
import           SSM.Core.Reference
import           SSM.Core.Syntax
import           SSM.Core.Type
import           SSM.Core.Backend
