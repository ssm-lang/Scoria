{-# LANGUAGE QuasiQuotes #-}

module TypeGen where

import qualified Data.Loc
import           Language.C.Quote.GCC
import qualified Language.C.Syntax    as C
import Text.PrettyPrint.Mainland.Class (pprList)
import Text.PrettyPrint.Mainland (pretty, Doc)

-- | Carries the intermediate code generation state gs::GenState where fst gs is
-- the list of top-level definitions for the generated header file, while snd gs
-- is the list of definitions for the generated object file
type GenState = ([C.Definition], [C.Definition])

initGen :: ([C.Definition], [C.Definition])
initGen = (initHeaders, [])
  where initHeaders =
          [ [cedecl| $esc:("#include <peng-base.h>")|]
          , [cedecl| typedef struct sv sv_t; |]
          , [cedecl| struct sv { $sdecls:svFields }; |]
          , [cedecl| static inline typename bool event_on(typename sv_t *v) { return v->last_updated == now; } |]
          , [cedecl| extern void assign_event(typename sv_t *, typename priority_t); |]
          , [cedecl| extern void later_event(typename sv_t *, typename peng_time_t); |]
          ]

prettyDoc :: ([C.Definition], [C.Definition]) -> (Doc, Doc)
prettyDoc (headers, code) = (pprList headers, pprList code)

mkScalarHeaders :: String -> String -> ([C.Definition], [C.Definition])
mkScalarHeaders typ fmtSpec = (genHeader, genCode)
  where svtyp       = "sv_" ++ typ ++ "_t"
        update      = "update_" ++ typ
        to_string   = "to_string_" ++ typ
        initialize  = "initialize_" ++ typ
        assign      = "assign_" ++ typ
        later       = "later_" ++ typ
        genHeader   =
          [ [cedecl|
              typedef struct sv {
                $sdecls:svFields
                typename $id:typ value;
                typename $id:typ event_value;
              } $id:svtyp;
            |]
          , [cedecl|
              extern void $id:to_string(typename sv_t *, char *, typename size_t);
            |]
          , [cedecl|
              extern void $id:initialize(typename $id:svtyp *);
            |]
          , [cedecl|
              extern void $id:assign(typename $id:svtyp, typename priority_t, typename $id:typ);
            |]
          , [cedecl|
              extern void $id:later(typename $id:svtyp, typename peng_time_t, typename $id:typ);
            |]
          ]
        genCode     =
          [ [cedecl|
              static void $id:update(typename sv_t *sv) {
                assert(sv);
                assert(sv->event_time == now);
                typename $id:svtyp *v = (typename $id:svtyp *) sv;
                v->value = v->event_value;
              }
            |]
          , [cedecl|
              static void $id:to_string(typename sv_t *sv, char *buffer, typename size_t size) {
                typename $id:svtyp *v = (typename $id:svtyp *) sv;
                char str[] = $string:(typ++" "++fmtSpec);
                snprintf(buffer, size, str, v->value);
              }
            |]
          , [cedecl|
              void $id:initialize(typename $id:svtyp *v) {
                assert(v);
                initialize_event((typename sv_t *) v);
                v->to_string = $id:to_string;
                v->update = $id:update;
              }
            |]
          , [cedecl|
              void $id:assign(typename $id:svtyp *v, typename priority_t priority, typename $id:typ value) {
                v->value = value;
                assign_event((typename sv_t *) v, priority);
              }
            |]
          , [cedecl|
              void $id:later(typename $id:svtyp *v, typename peng_time_t then, typename $id:typ value) {
                assert(v);
                v->event_value = value;
                later_event((typename sv_t *) v, time);
              }
            |]
          ]

-- | Fields shared across all scheduled variables
svFields :: [C.FieldGroup]
svFields = -- TODO: push update and to_string methods to a vtable
  [ [csdecl| void (*update)(typename sv_t *); |]
  , [csdecl| void (*to_string)(typename sv_t *, char *, typename size_t); |]
  , [csdecl| struct trigger *triggers; |]
  , [csdecl| typename peng_time_t last_updated; |]
  , [csdecl| typename peng_time_t event_time; |]
  ]
