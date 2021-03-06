module Pretty where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Data.List

import Core

type PPSt = ( Int       -- counter to generate fresh names
            , [String]  -- names of already generated functions
            , [String]  -- generated functions
            )
type PP a = StateT PPSt
              (ReaderT Int                   -- current level of indentation
                  (Writer [String])) a   -- output

showSSM :: Show a => SSM a -> String
showSSM ssma = let rd             = execStateT (printProcedure ssma) (0, [], [])
                   wr             = runReaderT rd 0
                   ((_,_,funs),_) = runWriter wr
               in unlines funs

printProcedure :: SSM a -> PP ()
printProcedure ssm@(Procedure n _ _) = do
    (_,wr,_) <- get
    if n `elem` wr
        then return ()
        else do modify $ \(i,na,o) -> (i, n:na, o)
                f <- censor (const []) $ snd <$> listen (local (const 0) (toString' ssm))
                modify $ \(i,na,o) -> (i,na,o ++ [unlines f])
  where
      toString' :: SSM a -> PP ()
      toString' ssm = case ssm of
          (Return x)        -> return ()
          (NewRef n e k)    -> do
              name <- case n of
                  Just (_,n') -> return n'
                  Nothing     -> do (i,w,b) <- get
                                    put (i+1,w,b)
                                    return $ "v" ++ show i
              emit $ name ++ " = NewRef " ++ show e
              toString' (k name)
          (SetRef r e k)    -> do
              emit $ "SetRef " ++ r ++ " = " ++ show e
              toString' (k ())
          (SetLocal e v k)  -> do
              emit $ "SetLocal " ++ show e ++ " = " ++ show v
              toString' (k ())
          (GetRef r s k)      -> do
              v <- case s of
                  Just (_, n) -> return (Var n)
                  Nothing     -> getExpString
              emit $ show v ++ " = GetRef " ++ r
              toString' (k v)
          (If c thn (Just els) k)  -> do
              emit $ "If " ++ show c
              emit   "Then"
              indent $ toString' thn
              emit   "Else"
              indent $ toString' els
              toString' (k ())
          (If c thn Nothing k)  -> do
              emit $ "If " ++ show c
              emit   "Then"
              indent $ toString' thn
              toString' (k ())
          (While c bdy k)   -> do
              emit $ "While " ++ show c
              indent $ toString' bdy
              toString' (k ())
          (After e r v k)   -> do
              emit $ "After " ++ show e ++ " " ++ r ++ " = " ++ show v
              toString' (k ())
          (Changed r s k)     -> do
              b <- case s of
                    (Just (_,n)) -> return (Var n)
                    Nothing      -> getExpString
              emit $ show b ++ " = @" ++ r
              toString' (k b) 
          (Wait vars k)     -> do
              emit $ "Wait [" ++ intercalate ", " vars ++ "]"
              toString' (k ())
          (Fork procs k)    -> do
              i <- ask
              {- the following two lines create the separator to use between the forked calls.
              This is used to make a fork rather than rendering like this
                  fork [f a b, f a b, f a b]
              render like this
                  fork [
                         f a b
                       , f a b
                       , f a b
                       ]
              which is a little easier to read. Also might look more like the haskell code (at least
              the way I like to write it).
              -}
              let indent = replicate (i + length "fork ") ' '
              let sep = '\n' : indent
              let calls = map printProcedureCall procs
              emit $ "Fork [ " ++ sep ++ "  " ++ intercalate (sep ++ ", ") calls ++ sep ++ "]"
              mapM_ printProcedure procs
              toString' (k ())
          (Procedure n _ k) -> do
              emit $ "Procedure " ++ n
              toString' (k ())
          (Argument n name (Left e) k)  -> do
              emit $ "Argument " ++ name
              toString' (k ())
          (Argument n name (Right r) k)  -> do
              emit $ "Argument &" ++ name
              toString' (k ())
          (Result n r k)    -> do
              emit $ "Result " ++ show r
              toString' (k ())
        
      getExpString :: PP SSMExp
      getExpString = do
          (i,_,_) <- get
          modify $ \(i,n,w) -> (i+1,n,w)
          return $ Var $ "v" ++ show i

      printProcedureCall :: SSM () -> String
      printProcedureCall (Procedure n _ k) = n ++ "(" ++ intercalate ", " args ++ ")"
          where
              getArgs :: SSM () -> [String]
              getArgs (Argument _ _ (Left e) k)  = show e : getArgs (k ())
              getArgs (Argument _ _ (Right n) k) = n      : getArgs (k ())
              getArgs _                          = []

              args :: [String]
              args = getArgs (k ())

      indent :: PP () -> PP ()
      indent pp = local (+2) pp

      emit :: String -> PP ()
      emit str = do
          ind <- ask
          tell [replicate ind ' ' ++ str]