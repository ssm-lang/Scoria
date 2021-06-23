{-# LANGUAGE QuasiQuotes #-}
module SSM.Backend.C.MainLoop where

import SSM.Core.LowSyntax
import SSM.Backend.C.Definitions
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
        $id:fork(($ty:act_t *) $id:enter($args:enterArgs));

        tick();
        DEBUG_PRINT("now %lu eventqueuesize %d\n", now, event_queue_len);
        for (;;) {
          now = next_event_time();
          if (now == NO_EVENT_SCHEDULED)
            break;
          tick();
          DEBUG_PRINT("now %lu eventqueuesize %d\n", now, event_queue_len);
        }

        /* Print the final values of the arguments declared earlier */
        $items:refPrints
      }
    |]
  ]
 where
  limit = maybe [cexp|ULONG_MAX|] (\i -> [cexp|$int:i|]) tickLimit

  enter = enter_ $ entry program
  enterArgs =
    [ [cexp|($ty:act_t *) &top|]
      , [cexp|PRIORITY_AT_ROOT|]
      , [cexp|DEPTH_AT_ROOT|]
      ]
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
      -- Args to the main SSM procedure are always given default values of 0.
    ]

  refPrints = map refPrint $ rights $ args program
  refPrint (ref, typ) =
    [citem|printf($string:fmtString, $id:fmtType, ($ty:fmtCast) $id:ref.value);|]
   where
    -- | We need to explicitly check if typ is an unsigned type to give it the
    -- appropriate formatter and cast.
    --
    -- TODO: once we move the formatters and type gen stuff into Haskell itself,
    -- we can just look it up from there.
    fmtString | typ `elem` [Ref TUInt64, Ref TUInt8] = "result " ++ ref ++ " %s %lu\n"
              | otherwise                    = "result " ++ ref ++ " %s %ld\n"
    fmtType = "str_" ++ typeId typ
    fmtCast | typ `elem` [Ref TUInt64, Ref TUInt8] = [cty|unsigned long|]
            | otherwise                    = [cty|long|]