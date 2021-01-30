module SSM where

import AST

import Data.List.NonEmpty hiding (unzip, zip)

type SSM a = IO a -- placeholder for _some_ monad, TBD at a later time (need to talk with Koen)

(*=) :: Ref a -> SSMExp a -> SSM SSMStm
r *= e = return $ Assign r e

valOf :: Ref a -> SSM a
valOf r = undefined