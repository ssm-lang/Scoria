-- | FIXME: add documentation about this regression test.
--
-- Use template below when appropriate.
--
-- Bug encountered: (what happened)
-- (Suspected) cause: (why it happened)
-- Fix: (suggestion/plan for how to fix)
-- Fixed: (include commit hash if already fixed; otherwise write 'notyet' or 'wontfix')
--
-- Include links to GitHub issues if any are created.
module Regression.Arb63Spec where

import Data.Map (fromList)
import SSM.Core
import qualified Test.SSM.Prop as T
import qualified Test.Hspec as H
import qualified Test.Hspec.QuickCheck as H

spec :: H.Spec
spec = T.correctSpec "Arb63" p

p :: Program
p = Program {initialQueueContent = [SSMProcedure (Ident {identName = "fun0", identSrcInfo = Nothing}) []], funs = fromList [(Ident {identName = "fun0", identSrcInfo = Nothing},Procedure {name = Ident {identName = "fun0", identSrcInfo = Nothing}, arguments = [], body = [Skip,Skip,Skip,Skip,Fork [(Ident {identName = "fun5", identSrcInfo = Nothing},[Left (Lit TInt32 (LInt32 1))])],Skip,Skip,Skip,Skip,Skip,Skip,Skip,Skip,Skip,Skip,Skip,Skip,Skip,Skip]}),(Ident {identName = "fun5", identSrcInfo = Nothing},Procedure {name = Ident {identName = "fun5", identSrcInfo = Nothing}, arguments = [(Ident {identName = "var4", identSrcInfo = Nothing},TInt32)], body = [Skip,Skip,Skip,Skip,Skip,Skip,Fork [(Ident {identName = "fun5", identSrcInfo = Nothing},[Left (BOp TInt32 (UOpE TInt32 (Var TInt32 (Ident {identName = "var4", identSrcInfo = Nothing})) Neg) (Lit TInt32 (LInt32 1743277089)) ORem)])],Skip]})], globalReferences = [(Ident {identName = "glob0", identSrcInfo = Nothing},Ref TInt32),(Ident {identName = "glob1", identSrcInfo = Nothing},Ref TEvent),(Ident {identName = "glob2", identSrcInfo = Nothing},Ref TUInt32),(Ident {identName = "glob3", identSrcInfo = Nothing},Ref TInt64),(Ident {identName = "glob4", identSrcInfo = Nothing},Ref TEvent)], peripherals = []}
