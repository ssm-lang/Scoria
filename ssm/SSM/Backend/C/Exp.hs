{-# LANGUAGE QuasiQuotes #-}
module SSM.Backend.C.Exp where

import SSM.Core.LowSyntax
import SSM.Backend.C.Definitions

import Language.C.Quote.GCC ( cexp )
import qualified Language.C.Syntax             as C

-- | Generate C expression from 'SSMExp' and a list of local variables.
genExp :: [String] -> SSMExp -> C.Exp
genExp _  (Var _ n              ) = [cexp|act->$id:n.value|]
genExp _  (Lit _ (LInt32  i    )) = [cexp|$int:i|]
genExp _  (Lit _ (LUInt8  i    )) = [cexp|$int:i|]
genExp _  (Lit _ (LInt64  i    )) = [cexp|(typename int64) $int:i|]
genExp _  (Lit _ (LUInt64 i    )) = [cexp|(typename uint64) $int:i|]
genExp _  (Lit _ (LBool   True )) = [cexp|true|]
genExp _  (Lit _ (LBool   False)) = [cexp|false|]
genExp ls (UOpE _ e Neg         ) = [cexp|- $exp:(genExp ls e)|]
genExp ls (UOpR _ (n, _) Changed)
  | n `elem` ls = [cexp|event_on(($ty:sv_t *) &act->$id:n)|]
  | otherwise   = [cexp|event_on(($ty:sv_t *) act->$id:n)|]
-- | Circumvent optimizations that take advantage of C's undefined signed
-- integer wraparound behavior. FIXME: remove this hack, which is probably not
-- robust anyway if C is aggressive about inlining.
genExp ls (BOp ty e1 e2 op)
  | ty == TInt32 && op == OPlus = [cexp|_add($exp:c1, $exp:c2)|]
  | otherwise                   = gen op
 where
  (c1, c2) = (genExp ls e1, genExp ls e2)
  gen OPlus  = [cexp|$exp:c1 + $exp:c2|]
  gen OMinus = [cexp|$exp:c1 - $exp:c2|]
  gen OTimes = [cexp|$exp:c1 * $exp:c2|]
  gen OLT    = [cexp|$exp:c1 < $exp:c2|]
  gen OEQ    = [cexp|$exp:c1 == $exp:c2|]