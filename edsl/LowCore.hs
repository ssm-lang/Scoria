module LowCore
     ( Stm(..)
     , Procedure(..)
     , Program(..)
     , transpile
     -- Re-export Core things so that other files only need to import LowCore
     , C.Type(..)
     , C.SSMExp(..)
     , C.SSMLit(..)
     , C.BinOp(..)
     , C.UnaryOp(..)
     , C.Reference(..)
     , C.Name(..)
     , C.expType
     , C.refType
     , C.mkReference
     , C.dereference
     , C.isReference
     , C.getVarName
     ) where

import qualified Core as C

import qualified Data.Map as Map
import Control.Monad.State.Lazy

data Stm = NewRef C.Name C.SSMExp C.SSMExp
         | GetRef C.Name C.SSMExp C.Reference
         | SetRef C.Reference C.SSMExp
         | SetLocal C.SSMExp C.SSMExp

         | If C.SSMExp [Stm] [Stm]
         | While C.SSMExp [Stm]
         | Skip

         | After C.SSMExp C.Reference C.SSMExp
         | Changed C.Name C.SSMExp C.Reference
         | Wait [C.Reference]
         | Fork [(String, [Either C.SSMExp C.Reference])]
  deriving Show

data Procedure = Procedure { name      :: String
                           , arguments :: [(String, C.Type)]
                           , body      :: [Stm]
                           }
  deriving Show

data Program = Program
             { main :: [Stm]
             , args :: [(String, C.Type)]
             , funs :: Map.Map String Procedure
             }
  deriving Show

type Transpile a = State TranspileState a

data TranspileState = TranspileState
                    { procedures :: Map.Map String Procedure
                    , generated  :: [String]
                    , mainargs   :: [(String, C.Type)]
                    }

transpile :: C.SSM () -> Program
transpile program = let (main, st) = runState comp state
                    in Program main (mainargs st) (procedures st)
  where
      state = TranspileState Map.empty [] []
      comp  = transpileProcedure $ C.runSSM program

transpileProcedure :: [C.SSMStm] -> Transpile [Stm]
transpileProcedure xs = fmap concat $ flip mapM xs $ \x -> case x of
    C.NewRef n e     -> return $ [NewRef n (C.Var (C.expType e) (C.getVarName n)) e]
    C.GetRef n r     -> return $ [GetRef n (C.Var (C.dereference (C.refType r)) (C.getVarName n)) r]
    C.SetRef r e     -> return $ [SetRef r e]
    C.SetLocal e1 e2 -> return $ [SetLocal e1 e2]

    C.If c thn els -> do thn' <- transpileProcedure (C.runSSM thn)
                         els' <- case els of
                             Just els' -> transpileProcedure (C.runSSM els')
                             Nothing -> return $ [Skip]
                         return $ [If c thn' els']
    C.While c bdy  -> transpileProcedure (C.runSSM bdy) >>= \bdy' -> return $ [While c bdy']
    
    C.After d r v -> return $ [After d r v]
    C.Changed n r -> return $ [Changed n (C.Var C.TBool (C.getVarName n)) r]
    C.Wait refs   -> return $ [Wait refs]
    C.Fork procs  -> do procs' <- mapM getCall procs
                        return $ [Fork procs']

    C.Procedure n    -> return []
    C.Argument n x a -> do let arginfo = (x, either C.expType C.refType a)
                           modify $ \st -> st { mainargs = mainargs st ++ [arginfo]}
                           return []
    C.Result n       -> return []
  where
      {- | Converts a `SSM ()` computation to a description of the call, and if necessary,
      this function will also update the environment to contain a mapping from the argument
      procedure to it's body. -}
      getCall :: C.SSM () -> Transpile (String, [Either C.SSMExp C.Reference])
      getCall ssm = do
          let stmts = C.runSSM ssm
          let name  = C.getProcedureName $ head stmts
          let (arginfo, args)  = unzip $ getArgs stmts
          st <- get
          if name `elem` generated st
              then return ()
              else do put $ st { generated = name : generated st }
                      nstmts <- transpileProcedure stmts
                      let fun = Procedure name arginfo nstmts
                      modify $ \st -> st { procedures = Map.insert name fun (procedures st)}
          return (name, args)

      {-| Return a tuple where the first component contains information about name
      and type about the arguments, and the second compoment is a list of the actual arguments. -}
      getArgs :: [C.SSMStm] -> [((String, C.Type), Either C.SSMExp C.Reference)]
      getArgs []                    = []
      getArgs (C.Procedure _: xs)   = getArgs xs
      getArgs (C.Argument _ x a:xs) = ((x, either C.expType C.refType a), a) : getArgs xs
      getArgs _                     = []