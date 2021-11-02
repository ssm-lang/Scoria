{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}

module SSM.Plugin
  ( plugin
  ) where

import Data.Generics (mkM, everywhereM, listify)

import GhcPlugins as GHC
import HsSyn      as GHC
import GhcPlugins as GHC hiding (Auto)
import FastString as FS
import OccName    as Name

----------------------------------------
-- The plugin driver, needs to be called exactly like this to work

plugin :: Plugin
plugin = defaultPlugin { parsedResultAction = const . ssm_plugin }

----------------------------------------
-- The plugin itself

ssm_plugin :: [CommandLineOption] -> HsParsedModule -> Hsc HsParsedModule
ssm_plugin cli parsed = do
  message "Plugin started!"
  let L loc hsMod = hpm_module parsed

  flags <- getDynFlags
  let wanted = extractAnn <$> listify (isAnn flags) hsMod
  message $ "Wanted: " ++ showPpr flags wanted
  hsMod' <- mkM (annotateBox flags wanted) `everywhereM` hsMod

  message "Plugin finished!"
  return parsed { hpm_module = L loc hsMod' }

----------------------------------------
-- Transform top-level function declarations annotated with EMBED

annotateBox :: DynFlags -> [RdrName] -> Match GhcPs (LHsExpr GhcPs) -> Hsc (Match GhcPs (LHsExpr GhcPs))
annotateBox flags wanted = \case
  match@(Match m_x m_ctx m_ps (GRHSs grhss_x lgrhss lbs))
    | isFunRhs m_ctx &&
      all isVarPat m_ps &&
      unLoc (mc_fun m_ctx) `elem` wanted -> do
        message $ "Transforming function:\n" ++ showPpr flags match

        let L _ fname = mc_fun m_ctx
        let argnames = fmap (\(L _ (VarPat _ (L _ v))) -> v) m_ps

        let body' = __box__
                    (showPpr flags fname)
                    (showPpr flags <$> argnames)
                    (lam m_ps lgrhss)

        let match' = Match m_x m_ctx [] (GRHSs grhss_x [noLoc (GRHS noExt [] body')] lbs)

        message $ "Into:\n" ++ showPpr flags match'
        return match'

  match -> return match

----------------------------------------
-- Wrappers

app :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
app x y = noLoc (HsApp noExt x y)

lam :: [LPat GhcPs] -> [LGRHS GhcPs (LHsExpr GhcPs)] -> LHsExpr GhcPs
lam pats lgrhss = noLoc (HsLam noExt (MG noExt (noLoc [noLoc (Match noExt LambdaExpr pats (GRHSs noExt lgrhss (noLoc (EmptyLocalBinds noExt))))]) Generated))

list :: [LHsExpr GhcPs] -> LHsExpr GhcPs
list exprs = noLoc (ExplicitList noExt Nothing exprs)

(&) :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
(&) = app
infixl 4 &

paren :: LHsExpr GhcPs -> LHsExpr GhcPs
paren x = noLoc (HsPar noExt x)

var :: RdrName -> LHsExpr GhcPs
var x = noLoc (HsVar noExt (noLoc x))

varP :: RdrName -> LPat GhcPs
varP x = noLoc (VarPat noExt (noLoc x))


strLit :: String -> LHsExpr GhcPs
strLit s = noLoc (HsLit noExt (HsString NoSourceText (fsLit s)))

__box__ :: String -> [String] -> LHsExpr GhcPs -> LHsExpr GhcPs
__box__ fname argnames body =
  var (mkRdrName "box")
  & strLit fname
  & list (strLit <$> argnames)
  & paren body

----------------------------------------
-- Helpers

__EMBED__ :: RdrName
__EMBED__ = mkRdrName "EMBED"

-- | Check whether an annotation pragma is of the shape:
-- | {-# ANN ident SrcInfo #-}
pattern HsAnn :: RdrName -> RdrName -> AnnDecl GhcPs
pattern HsAnn lhs rhs <-
  HsAnnotation _ _
  (ValueAnnProvenance (L _ lhs))
  (L _ (HsVar _ (L _ rhs)))

isAnn :: DynFlags -> AnnDecl GhcPs -> Bool
isAnn flags (HsAnn _ rhs) = showPpr flags rhs == showPpr flags __EMBED__
isAnn _     _             = False

extractAnn :: AnnDecl GhcPs -> RdrName
extractAnn (HsAnn target _) = target
extractAnn _                = error "this should not happen"

-- | Is this pattern a variable?
isVarPat :: LPat GhcPs -> Bool
isVarPat (L _ (VarPat {})) = True
isVarPat _                 =  False

-- | Is this a patter matching an argument of a function binding?
isFunRhs :: HsMatchContext id -> Bool
isFunRhs (FunRhs {}) = True
isFunRhs _           = False

-- | Create a name from a string
mkRdrName :: String -> RdrName
mkRdrName = mkUnqual Name.varName . mkFastString

-- | Print a message to the console
message :: String -> Hsc ()
message str = liftIO $ putStrLn $ "[SSM] " ++ str
