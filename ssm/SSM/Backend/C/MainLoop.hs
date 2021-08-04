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
        initialize_global_variables();
        
        $ty:act_t top = { .step = $id:top_return };

        /* Initialize variables to be passed to the main SSM procedure */
        $items:argInits

        /* Enter main SSM procedure */
        $id:fork($id:enter($args:enterArgs));
        $ty:time_t next;
        do {
          tick();
          next = next_event_time();
          set_now(next);
        } while (next != NO_EVENT_SCHEDULED);

        /* Print the final values of the arguments declared earlier */
        $items:refPrints
      }
    |]
  ]
 where
  limit = maybe [cexp|ULONG_MAX|] (\i -> [cexp|$int:i|]) tickLimit

  enter = enter_ $ identName $ entry program
  enterArgs =
      [[cexp|&top|], [cexp|PRIORITY_AT_ROOT|], [cexp|DEPTH_AT_ROOT|]]
      ++ map enterArg (args program)
  enterArg (Left  ssmExp  ) = genExp [] ssmExp
  -- ^ TODO: this is buggy if ssmExp contains a var?? Maybe double check what's
  -- going on with that.
  enterArg (Right r) = [cexp|&$id:(refName r)|]

  argInits = concatMap argInit $ rights $ args program
  argInit r =
    [ [citem|$ty:(svt_ (refType r)) $id:(refName r);|]
    , [citem|$id:(initialize_ (refType r))(&$id:(refName r));|]
    , [citem|$id:(refName r).value = 0;|]
    , [citem|DEBUG_SV_SET_VAR_NAME($id:(refName r).sv.debug, $string:(refName r));|]
      -- Args to the main SSM procedure are always given default values of 0.
    ]
    where
      ref = refName r
      typ = refType r

  refPrints = map refPrint $ rights $ args program
  refPrint r = [citem|
    DEBUG_PRINT("result %s %s %s\n", DEBUG_SV_GET_VAR_NAME($id:(refName r).sv.debug),
                                     DEBUG_SV_GET_TYPE_NAME($id:(refName r).sv.debug),
                                     DEBUG_SV_GET_VALUE_REPR($id:(refName r).sv.debug,
                                     &$id:(refName r).sv));|]
