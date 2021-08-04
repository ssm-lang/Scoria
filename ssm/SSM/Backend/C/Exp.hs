{-# LANGUAGE QuasiQuotes #-}
module SSM.Backend.C.Exp
    ( {- | @genExp@ generates C expressions that represent a @Exp a@ in the
      SSM language. Since expressions may refer to variable expressions, an
      environment containing the variables in scope must be supplied. -}
      genExp
    ) where

import SSM.Core.Syntax
import SSM.Backend.C.Identifiers
import SSM.Backend.C.Types

import Language.C.Quote.GCC ( cexp )
import qualified Language.C.Syntax             as C

-- | Generate C expression from 'SSMExp' and a list of local variables.
genExp :: [Reference] -> SSMExp -> C.Exp
genExp _  (Var t n              )
  | baseType t == TEvent = [cexp|0|]
  | otherwise            = [cexp|acts->$id:(identName n).value|]
genExp _  (Lit _ (LInt32  i    )) = [cexp|$int:i|]
genExp _  (Lit _ (LUInt8  i    )) = [cexp|$int:i|]
genExp _  (Lit _ (LInt64  i    )) = [cexp|(typename i64) $int:i|]
genExp _  (Lit _ (LUInt64 i    )) = [cexp|(typename u64) $int:i|]
genExp _  (Lit _ (LBool   True )) = [cexp|true|]
genExp _  (Lit _ (LBool   False)) = [cexp|false|]
genExp _  (Lit _ (LEvent       )) = [cexp|0|]
genExp ls (UOpE _ e Neg         ) = [cexp|- $exp:(genExp ls e)|]
genExp ls (UOpR _ r Changed) = [cexp|$id:event_on($exp:(refSV r ls))|]
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
