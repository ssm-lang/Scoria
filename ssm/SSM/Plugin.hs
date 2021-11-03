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
import Data.List
import Data.Maybe
import Debug.Trace

----------------------------------------
-- The plugin driver, needs to be called exactly like this to work

plugin :: Plugin
plugin = defaultPlugin { parsedResultAction = const . ssm_plugin }

----------------------------------------
-- The plugin itself

ssm_plugin :: [CommandLineOption] -> HsParsedModule -> Hsc HsParsedModule
ssm_plugin cli parsed = do
  message "plugin started!"
  let L loc hsMod = hpm_module parsed
  flags <- getDynFlags

  mode <- runMode cli
  case mode of
    Annotate -> do
      let wanted = extractAnn <$> listify (isAnn flags) hsMod
      message $ "should box: " ++ showPpr flags wanted
      hsMod' <- mkM (annotateBox flags wanted) `everywhereM` hsMod

      message "plugin finished!"
      return parsed { hpm_module = L loc hsMod' }
    Routine -> do
      hsMod' <- mkM (routineBox flags) `everywhereM` hsMod
      message "plugin finished!"
      return parsed {hpm_module = L loc hsMod' }

----------------------------------------
-- Transform top-level function declarations annotated with ROUTINE

annotateBox :: DynFlags -> [RdrName] -> Match GhcPs (LHsExpr GhcPs) -> Hsc (Match GhcPs (LHsExpr GhcPs))
annotateBox flags wanted = \case
  {- somewhat dangerous pattern match on the body, but it should work for our
     simple use cases for now. -}
  match@(Match m_x m_ctx m_ps (GRHSs grhss_x lgrhss@[L _ (GRHS _ _ body)] lbs))
    | isFunRhs m_ctx &&
      unLoc (mc_fun m_ctx) `elem` wanted -> do
        message $ "Transforming function:\n" ++ showPpr flags match

        let L _ fname = mc_fun m_ctx
        let argnames = fmap (\(L _ (VarPat _ (L _ v))) -> v) m_ps

        let body' = if null argnames
              then __boxNullary__
                   (showPpr flags fname)
                   body
              else __box__
                   (showPpr flags fname)
                   (showPpr flags <$> argnames)
                   (lam m_ps lgrhss)

        let match' = Match m_x m_ctx [] (GRHSs grhss_x [noLoc (GRHS noExt [] body')] lbs)

        message $ "Into:\n" ++ showPpr flags match'
        return match'

  match -> return match

----------------------------------------
-- Transform top-level function declarations tagged with routine

routineBox :: DynFlags -> Match GhcPs (LHsExpr GhcPs) -> Hsc (Match GhcPs (LHsExpr GhcPs))
routineBox flags = \case
  {- somewhat dangerous pattern match on the body, but it should work for our
     simple use cases for now. -}
  match@(Match m_x m_ctx m_ps (GRHSs grhss_x lgrhss@[L _ (GRHS _ _ body)] lbs))
    | isFunRhs m_ctx -> do
        iate <- isAppliedToRoutine body
        case iate of
          Just arg -> do
            message $ "Transforming function:\n" ++ showPpr flags match

            let L _ fname = mc_fun m_ctx
            let argnames  = fmap (\(L _ (VarPat _ (L _ v))) -> v) m_ps
            let body'     = if null argnames
                  then __boxNullary__
                       (showPpr flags fname)
                       arg
                  else __box__
                       (showPpr flags fname)
                       (showPpr flags <$> argnames)
                       (lam m_ps [noLoc (GRHS noExt [] arg)])

            let match' = Match m_x m_ctx [] (GRHSs grhss_x [noLoc (GRHS noExt [] body')] lbs)

            message $ "Into:\n" ++ showPpr flags match'
            return match'
          Nothing -> return match

  match -> return match

isAppliedToRoutine :: LHsExpr GhcPs -> Hsc (Maybe (LHsExpr GhcPs))
-- routine thebody
isAppliedToRoutine (L _ (HsApp _ (L _ (HsVar _ (L _ id))) arg))
  | id == __routine__ = return $ Just arg
-- routine $ thebody
isAppliedToRoutine (L _ (OpApp _ (L _ (HsVar _ (L _ id))) _ arg))
  | id == __routine__ = return $ Just arg
isAppliedToRoutine (L _ (OpApp _ f op arg)) = do
  res <- isAppliedToRoutine f
  case res of
    Just f' -> return $ Just $ noLoc $ OpApp noExt f' op arg
    Nothing -> return Nothing
isAppliedToRoutine _ = return Nothing

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

__boxNullary__ :: String -> LHsExpr GhcPs -> LHsExpr GhcPs
__boxNullary__ fname body =
  var (mkRdrName "boxNullary")
  & strLit fname
  & paren body
----------------------------------------
-- Helpers

__routine__ :: RdrName
__routine__ = mkRdrName "routine"

__ROUTINE__ :: RdrName
__ROUTINE__ = mkRdrName "ROUTINE"

-- | Check whether an annotation pragma is of the shape:
-- | {-# ANN ident SrcInfo #-}
pattern HsAnn :: RdrName -> RdrName -> AnnDecl GhcPs
pattern HsAnn lhs rhs <-
  HsAnnotation _ _
  (ValueAnnProvenance (L _ lhs))
  (L _ (HsVar _ (L _ rhs)))

isAnn :: DynFlags -> AnnDecl GhcPs -> Bool
isAnn flags (HsAnn _ rhs) = showPpr flags rhs == showPpr flags __ROUTINE__
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

-- | The mode with which to transform definitions
data RunMode
  = Annotate  -- ^ Definitions are annotated
  | Routine   -- ^ Definitions are tagged with 'routine'

runMode :: [String] -> Hsc RunMode
runMode opts = tomode $ filter ("mode=" `isPrefixOf`) opts
  where
    tomode :: [String] -> Hsc RunMode
    tomode [] = do
      message "no plugin mode selected, defaulting to annotation mode"
      return Annotate
    tomode ["mode=annotate"] = do
      message "annotation mode selected"
      return Annotate
    tomode ["mode=routine"]    = do
      message "routine mode selected"
      return Routine
    tomode othermode = do
      message "unrecognized mode detected, defaulting to annotation mode"
      return Annotate
