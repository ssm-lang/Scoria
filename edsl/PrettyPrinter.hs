{-# LANGUAGE GADTs #-}
module PrettyPrinter where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer

import Data.IORef
import Data.List
import AST

type PP a = StateT Int                 -- counter to generate fresh names
              (ReaderT Int             -- current level of indentation
                  (Writer [String])) a -- output

showSSM :: Show a => SSM a -> String
showSSM ssma = let rd = evalStateT (toString' ssma) 0
                   wr = runReaderT rd 0
                   w  = execWriter wr
               in unlines w

{-
NOTE: When generating a 'reference' to prettyprint the actual IORef is left as undefined.
This works because to prettyprint it is not necessary to inspect it, only the variable name.
-}

toString' :: SSM a -> PP ()
toString' ssm = case ssm of
    (Return x)        -> return ()
    (NewRef n e k)    -> do
        r@(n,_) <- getRefString
        emit $ n ++ " = NewRef " ++ show e
        toString' (k r)
    (SetRef (r,_) e k)    -> do
        emit $ "SetRef " ++ r ++ " = " ++ show e
        toString' (k ())
    (GetRef (r,_) k)      -> do
        v <- getExpString
        emit $ show v ++ " = GetRef " ++ r
        toString' (k v)
    (If c thn (Just els) k)  -> do
        emit $ "If " ++ show c
        emit $ "Then"
        indent $ toString' thn
        emit $ "Else"
        indent $ toString' els
        toString' (k ())
    (If c thn Nothing k)  -> do
        emit $ "If " ++ show c
        emit $ "Then"
        indent $ toString' thn
        toString' (k ())
    (While c bdy k)   -> do
        emit $ "While " ++ show c
        indent $ toString' bdy
        toString' (k ())
    (After e (r,_) v k)   -> do
        emit $ "After " ++ show e ++ " " ++ r ++ " = " ++ show v
        toString' (k ())
    (Changed (r,_) k)     -> do
        b <- getExpString
        emit $ show b ++ " = @" ++ r
        toString' (k b) 
    (Wait vars k)     -> do
        emit $ "Wait [" ++ intercalate ", " (map fst vars) ++ "]"
        toString' (k ())
    (Fork procs k)    -> do
        emit $ "Fork [" ++ intercalate "," (map printProcedureCall procs) ++ "]"
        toString' (k ())
    (Procedure n _ k) -> do
        emit $ "Procedure " ++ n
        toString' (k ())
    (Argument n (Left e) k)  -> do
        emit $ "Argument " ++ show e
        toString' (k ())
    (Argument n (Right (r,_)) k)  -> do
        emit $ "Argument &" ++ r
        toString' (k ())
    (Result n r k)    -> do
        emit $ "Result " ++ show r
        toString' (k ())
  where
      getExpString :: PP SSMExp
      getExpString = do
          i <- get
          put $ i + 1
          return $ Var $ "v" ++ show i

      -- undefined for now
      getRefString :: PP (String, IORef SSMExp)
      getRefString = do
          i <- get
          put $ i + 1
          return $ ("v" ++ show i, undefined)

      printProcedureCall :: SSM () -> String
      printProcedureCall (Procedure n _ k) = n ++ "(" ++ intercalate ", " args ++ ")"
        where
            getArgs :: SSM () -> [String]
            getArgs (Argument _ (Left e) k)      = (show e) : getArgs (k ())
            getArgs (Argument _ (Right (n,_)) k) = n : getArgs (k ())
            getArgs _                            = []

            args :: [String]
            args = getArgs (k ())

      indent :: PP () -> PP ()
      indent pp = local (+4) pp

      emit :: String -> PP ()
      emit str = do
          ind <- ask
          tell [replicate ind ' ' ++ str]

      result :: String -> PP String
      result str = do
          ind <- ask
          return $ replicate ind ' ' ++ str