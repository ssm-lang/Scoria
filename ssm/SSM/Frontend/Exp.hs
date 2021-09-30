{- | This module exposes the `Exp` type that is used to talk about expressions in the
frontend EDSL -}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SSM.Frontend.Exp
    ( Exp(..)
    ) where

import           SSM.Frontend.Box
import           SSM.Frontend.Syntax

import           BinderAnn.Monadic

import           Control.Monad.State
import           Data.Int
import           Data.Proxy
import           Data.Word

-- | Expressions of a particular type
newtype Exp a = Exp SSMExp
  deriving Show

-- | Arguments we can apply SSM procedures to
instance Arg (Exp a) where
    arg _    []       _       = error "No more parameter names"
    arg name (x : xs) (Exp b) = do
        emit $ Argument (Ident name Nothing) (Ident x Nothing) (Left b)
        return $ (Exp (Var (expType b) (Ident x Nothing)), xs)

-- | BinderAnn instance for `Exp`, so that we can capture source information
instance AnnotatedM SSM (Exp a) where
    annotateM ma info = do
        v     <- ma
        stmts <- gets statements
        let stmt  = last stmts
        let stmt' = renameStmt stmt $ let (Info n i) = info in (n, i)
        modify $ \st -> st { statements = init stmts ++ [stmt'] }
        return $ renameExp v info
      where
          -- | Rename expression with source information
          renameExp :: Exp a -> SrcInfo -> Exp a
          renameExp e (Info Nothing  _      ) = e
          renameExp e (Info _        Nothing) = e
          renameExp e (Info (Just n) info   ) = case e of
              Exp (Var t _) -> Exp $ Var t (Ident n info)
              _             -> e

-- | Literals
newtype Lit a = FLit SSMLit    -- ^ literals
  deriving Show

{- | Class of types that can be constructed using literals.
This doesn't really work very well now, so we need to figure out some
better way of doing this. -}
class FromLiteral a where
    fromLit :: a -> Lit a

instance FromLiteral Int32 where
    fromLit i = FLit $ LInt32 (fromIntegral i)

instance FromLiteral Int64 where
    fromLit i = FLit $ LInt64 (fromIntegral i)

instance FromLiteral Word64 where
    fromLit i = FLit $ LUInt64 (fromIntegral i)

instance FromLiteral Word32 where
    fromLit i = FLit $ LUInt32 (fromIntegral i)

instance FromLiteral Word8 where
    fromLit i = FLit $ LUInt8 (fromIntegral i)

instance (Num a, FromLiteral a, SSMType a) => Num (Exp a) where
    e@(Exp e1) + (Exp e2) = Exp $ BOp (typeOf e) e1 e2 OPlus
    e@(Exp e1) - (Exp e2) = Exp $ BOp (typeOf e) e1 e2 OMinus
    e@(Exp e1) * (Exp e2) = Exp $ BOp (typeOf e) e1 e2 OTimes
    fromInteger i =
        let FLit l = fromLit (fromInteger @a i)
        in  Exp $ Lit (typeOf (Proxy @a)) l
    abs _ = undefined
    signum _ = undefined
