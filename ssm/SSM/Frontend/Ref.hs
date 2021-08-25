{-# LANGUAGE MultiParamTypeClasses #-}
module SSM.Frontend.Ref where

import           SSM.Frontend.Box
import           SSM.Frontend.Syntax

import           BinderAnn.Monadic

import           Control.Monad.State

newtype Ref a = Ptr Reference
  deriving Show

instance Arg (Ref a) where
    arg _    []       _       = error "No more parameter names"
    arg name (x : xs) (Ptr r) = do
        emit $ Argument (Ident name Nothing) (Ident x Nothing) $ Right r
        return (Ptr $ SSM.Frontend.Syntax.renameRef r (Ident x Nothing), xs)

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
