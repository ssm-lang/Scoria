{-# LANGUAGE QuasiQuotes #-}
module SSM.Backend.C.MainLoop ( genMain ) where

import SSM.Core.Syntax
import SSM.Backend.C.Identifiers
import SSM.Backend.C.Exp

import Data.Either

import           Language.C.Quote.GCC
import qualified Language.C.Syntax             as C

{- THIS MODULE NEEDS TO BE REFACTORED - we should generate a C agnostic main -}

-- | Generate C definition for the main program and the top_return function,
-- both to be placed at the bottom of the generated C (or at least after the
-- type definitions and function prototypes).
genMain :: Program -> Maybe Int -> [C.Definition]
genMain program tickLimit =
  [ [cedecl| /** Used by DEBUG_PRINT as a microtick threshold */
             $ty:time_t limit = $exp:limit;|]
  , [cedecl| void $id:top_return($ty:act_t *act) { return; } |]
  , [cedecl|
      int main(void) {
        $ty:act_t top = { .step = $id:top_return };

        /* Enter main SSM procedure */
        $id:fork($id:enter($args:enterArgs));
        $ty:time_t next;
        do {
          tick();
          next = next_event_time();
          set_now(next);
        } while (next != NO_EVENT_SCHEDULED);

      }
    |]
  ]
 where
  limit = maybe [cexp|ULONG_MAX|] (\i -> [cexp|$int:i|]) tickLimit

  enter = enter_ $ identName $ entry program
  enterArgs =
      [[cexp|&top|], [cexp|PRIORITY_AT_ROOT|], [cexp|DEPTH_AT_ROOT|]]
