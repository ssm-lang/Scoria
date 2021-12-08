{-# LANGUAGE TypeFamilies #-}
module SSM.Core.Backend where

import qualified Language.C.Syntax as C

data C

type family Definition backend where
    Definition C = C.Definition

type family Initialization backend where
    Initialization C = C.BlockItem

type family Schedule backend where
    Schedule C = C.BlockItem
