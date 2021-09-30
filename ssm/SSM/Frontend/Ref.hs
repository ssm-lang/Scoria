{- | This module exposes the `Ref` type that is used to talk about references in the
frontend EDSL -}
{-# LANGUAGE MultiParamTypeClasses #-}
module SSM.Frontend.Ref where

import           SSM.Frontend.Box
import           SSM.Frontend.Syntax

import           BinderAnn.Monadic

import           Control.Monad.State

-- | References of a particular type
newtype Ref a = Ptr Reference
  deriving Show

-- | We can use `Reference`s as arguments to our SSM procedures
instance Arg (Ref a) where
    arg _    []       _       = error "No more parameter names"
    arg name (x : xs) (Ptr r) = do
        emit $ Argument (Ident name Nothing) (Ident x Nothing) $ Right r

        {- from this point and onwards, the reference exists in the context of a
        procedure, so we need to convert it to a dynamic reference, regardless
        of what it was before. -}
        let newref = makeDynamicRef (Ident x Nothing) $ refType r
        return (Ptr newref, xs)

-- | We can capture names for `Reference`s
instance AnnotatedM SSM (Ref a) where
    annotateM ma info = do
        v     <- ma
        stmts <- gets statements
        let stmt  = last stmts
        let stmt' = renameStmt stmt $ let (Info n i) = info in (n, i)
        modify $ \st -> st { statements = init stmts ++ [stmt'] }
        return $ SSM.Frontend.Ref.renameRef v info

renameRef :: Ref a -> SrcInfo -> Ref a
renameRef e (Info Nothing _      ) = e
renameRef e (Info _       Nothing) = e
renameRef (Ptr r) (Info (Just n) l) =
    Ptr $ SSM.Frontend.Syntax.renameRef r (Ident n l)
