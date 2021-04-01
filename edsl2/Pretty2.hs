module Pretty2 where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Data.List

import Core2

type PPSt = ( [String]  -- names of already generated functions
            , [String]  -- generated functions
            )
type PP a = StateT PPSt
              (ReaderT Int               -- current level of indentation
                  (Writer [String])) a   -- output

showSSM :: Show a => SSM a -> String
showSSM ssma = let rd           = execStateT (printProcedure (runSSM ssma)) ([], [])
                   wr           = runReaderT rd 0
                   ((_,funs),_) = runWriter wr
               in unlines funs

printProcedure :: [SSMStm] -> PP ()
printProcedure [] = return ()
printProcedure ssm@(Procedure n:_) = do
    (wr,_) <- get
    if n `elem` wr
        then return ()
        else do modify $ \(na,o) -> (n:na, o)
                f <- censor (const []) $ snd <$> listen (local (const 0) (toString' ssm))
                modify $ \(na,o) -> (na,o ++ [unlines f])
  where
      toString' :: [SSMStm] -> PP ()
      toString' []        = return ()
      toString' (ssm:xs) = case ssm of
          (NewRef n e)    -> do
              let name = getVarName n
              emit $ name ++ " = NewRef " ++ show e
              toString' xs
          (SetRef (r,_) e)    -> do
              emit $ "SetRef " ++ r ++ " = " ++ show e
              toString' xs
          (SetLocal e v)  -> do
              emit $ "SetLocal " ++ show e ++ " = " ++ show v
              toString' xs
          (GetRef (r,t) n)      -> do
              let name = getVarName n
              emit $ name ++ " = GetRef " ++ r
              toString' xs
          (If c thn (Just els))  -> do
              emit $ "If " ++ show c
              emit   "Then"
              indent $ toString' $ runSSM thn
              emit   "Else"
              indent $ toString' $ runSSM els
              toString' xs
          (If c thn Nothing)  -> do
              emit $ "If " ++ show c
              emit   "Then"
              indent $ toString' $ runSSM thn
              toString' xs
          (While c bdy)   -> do
              emit $ "While " ++ show c
              indent $ toString' $ runSSM bdy
              toString' xs
          (After e (r,_) v)   -> do
              emit $ "After " ++ show e ++ " " ++ r ++ " = " ++ show v
              toString' xs
          (Changed (r,_) n)   -> do
              let name = getVarName n
              emit $ name ++ " = @" ++ r
              toString' xs
          (Wait vars)     -> do
              emit $ "Wait [" ++ intercalate ", " (map fst vars) ++ "]"
              toString' xs
          (Fork procs)    -> do
              i <- ask
              let indent = replicate (i + length "fork ") ' '
              let sep = '\n' : indent
              let calls = map printProcedureCall procs
              emit $ "Fork [ " ++ sep ++ "  " ++ intercalate (sep ++ ", ") calls ++ sep ++ "]"
              mapM_ (printProcedure . runSSM) procs
              toString' xs
          (Procedure n) -> do
              emit $ "Procedure " ++ n
              toString' xs
          (Argument n name (Left e))  -> do
              emit $ "Argument " ++ name ++ " = " ++ show e
              toString' xs
          (Argument n name (Right r))  -> do
              emit $ "Argument &" ++ name ++ " = " ++ fst r
              toString' xs
          (Result n)    -> do
              emit $ "Result"
              toString' xs
        
      printProcedureCall :: SSM () -> String
      printProcedureCall ssm = name ++ "(" ++ intercalate ", " args ++ ")"
          where
              stmts = runSSM ssm

              name :: String
              name = getProcedureName (head stmts)

              getArgs :: [SSMStm] -> [String]
              getArgs (Argument _ _ (Left e):xs)      = show e : getArgs xs
              getArgs (Argument _ _ (Right (n,_)):xs) = n      : getArgs xs
              getArgs _                               = []

              args :: [String]
              args = getArgs (tail stmts)

      indent :: PP () -> PP ()
      indent pp = local (+2) pp

      emit :: String -> PP ()
      emit str = do
          ind <- ask
          tell [replicate ind ' ' ++ str]