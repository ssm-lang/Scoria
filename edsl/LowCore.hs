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
     , C.UnaryOpE(..)
     , C.UnaryOpR(..)
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

data Stm = NewRef C.Name C.Type C.SSMExp
         | GetRef C.Name C.Type C.Reference
         | SetRef C.Reference C.SSMExp
         | SetLocal C.Name C.Type C.SSMExp

         | If C.SSMExp [Stm] [Stm]
         | While C.SSMExp [Stm]
         | Skip

         | After C.SSMExp C.Reference C.SSMExp
         | Wait [C.Reference]
         | Fork [(String, [Either C.SSMExp C.Reference])]
  deriving (Show, Eq, Read)

data Procedure = Procedure
  { -- | Name of the procedure.
    name      :: String
    -- | Parameter names and types of the procedure.
  , arguments :: [(String, C.Type)]
    -- | Statements that make up this procedure.
  , body      :: [Stm]
  }
  deriving (Eq, Show, Read)

data Program = Program
  { -- | Name of the procedure that is the program entrypoint.
    entry :: String
    -- | Arguments the entrypoint was applied to.
  , args :: [Either C.SSMExp C.Reference]
    -- | Map that associates procedure names with their definitions.
  , funs :: Map.Map String Procedure
  }
  deriving (Show, Read)

type Transpile a = State TranspileState a

data TranspileState = TranspileState
  { -- | Map that associate procedure names with their Procedure definition.
    procedures  :: Map.Map String Procedure
    -- | List of procedure names that have already been generated.
  , generated   :: [String]
    -- | Name of the procedure that was the program entrypoint?
    -- This is the first that that will be populated in the state during execution.
  , mainname    :: Maybe String
    -- | Parameter names and types of the program entrypoint.
  , mainargs    :: [(String, C.Type)]
    -- | What values were the program entrypoint applied to?
  , mainargvals :: [Either C.SSMExp C.Reference]
  }

transpile :: C.SSM () -> Program
transpile program = case mainname st of

    Nothing -> error $ "no name found for the program entrypoint - did you forget to use box?"

    Just n  -> if n `elem` (Map.keys $ procedures st)
      then Program n (mainargvals st) (procedures st)
      else let p = (Procedure n (mainargs st) main)
           in Program n (mainargvals st) $ Map.insert n p (procedures st)
  where
      (main,st) = runState comp state
      state     = TranspileState Map.empty [] Nothing [] []
      comp      = transpileProcedure $ C.runSSM program

transpileProcedure :: [C.SSMStm] -> Transpile [Stm]
transpileProcedure xs = fmap concat $ flip mapM xs $ \x -> case x of
    C.NewRef n e     -> return $ [NewRef n (C.expType e) e]
    C.GetRef n r     -> return $ [GetRef n (C.dereference (C.refType r)) r]
    C.SetRef r e     -> return $ [SetRef r e]
    C.SetLocal (C.Var t n) e2 -> return $ [SetLocal (C.Fresh n) t e2]
    C.SetLocal _ _ -> error "Trying to set a local variable that is not a variable"

    C.If c thn els -> do
      thn' <- transpileProcedure (C.runSSM thn)
      els' <- case els of
                Just els' -> transpileProcedure (C.runSSM els')
                Nothing -> return $ [Skip]
      return $ [If c thn' els']
    C.While c bdy  -> transpileProcedure (C.runSSM bdy) >>= \bdy' -> return $ [While c bdy']
    
    C.After d r v -> return $ [After d r v]
    C.Wait refs   -> return $ [Wait refs]
    C.Fork procs  -> do
      procs' <- mapM getCall procs
      return $ [Fork procs']

    C.Procedure n    -> do
      -- Only update the mainname value if it is `Nothing`, otherwise keep
      -- the previous value.
      modify $ \st -> st { mainname = maybe (Just n) Just $ mainname st }
      return []
    C.Argument n x a -> do 
      let arginfo = (x, either C.expType C.refType a)
      let a'      = either (Left) (Right . (,) x . snd) a
      modify $ \st -> st { mainargs    = mainargs st ++ [arginfo]
                         , mainargvals = mainargvals st ++ [a']
                         }
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
                      nstmts <- recursive $ transpileProcedure stmts
                      let fun = Procedure name arginfo nstmts
                      modify $ \st -> st { procedures = Map.insert name fun (procedures st) }
          return (name, args)

      recursive :: Transpile a -> Transpile a
      recursive tr = do
        old <- get
        a <- tr
        modify $ \st -> st { mainname    = mainname old
                           , mainargs    = mainargs old
                           , mainargvals = mainargvals old}
        return a

      {-| Return a tuple where the first component contains information about name
      and type about the arguments, and the second compoment is a list of the actual arguments. -}
      getArgs :: [C.SSMStm] -> [((String, C.Type), Either C.SSMExp C.Reference)]
      getArgs []                    = []
      getArgs (C.Procedure _: xs)   = getArgs xs
      getArgs (C.Argument _ x a:xs) = ((x, either C.expType C.refType a), a) : getArgs xs
      getArgs _                     = []
