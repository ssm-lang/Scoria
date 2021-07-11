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

        /* Initialize variables to be passed to the main SSM procedure */
        $items:argInits

        /* Enter main SSM procedure */
        $id:fork($id:enter($args:enterArgs));
        do {
          ssm_tick();
        } while (ssm_next_event_time() != SSM_NEVER);

        /* Print the final values of the arguments declared earlier */
        $items:refPrints
      }
    |]
  ]
 where
  limit = maybe [cexp|ULONG_MAX|] (\i -> [cexp|$int:i|]) tickLimit

  enter = enter_ $ entry program
  enterArgs =
      [[cexp|&top|], [cexp|SSM_ROOT_PRIORITY|], [cexp|SSM_ROOT_DEPTH|]]
      ++ map enterArg (args program)
  enterArg (Left  ssmExp  ) = genExp [] ssmExp
  -- ^ TODO: this is buggy if ssmExp contains a var?? Maybe double check what's
  -- going on with that.
  enterArg (Right (ref, _)) = [cexp|&$id:ref|]

  argInits = concatMap argInit $ rights $ args program
  argInit (ref, typ) =
    [ [citem|$ty:(svt_ typ) $id:ref;|]
    , [citem|$id:(initialize_ typ)(&$id:ref);|]
    , [citem|$id:ref.value = 0;|]
    , [citem|DEBUG_SV_SET_VAR_NAME($id:ref.sv.debug, $string:ref);|]
      -- Args to the main SSM procedure are always given default values of 0.
    ]

  refPrints = map refPrint $ rights $ args program
  refPrint (ref, typ) = [citem|
    DEBUG_PRINT("result %s %s %s\n", DEBUG_SV_GET_VAR_NAME($id:ref.sv.debug),
                                     DEBUG_SV_GET_TYPE_NAME($id:ref.sv.debug),
                                     DEBUG_SV_GET_VALUE_REPR($id:ref.sv.debug,
                                     &$id:ref.sv));|]
