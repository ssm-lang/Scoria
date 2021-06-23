{-| This module exports a syntax of a lower level, which is a bit more flat.
the intention is that this kind of syntax is the one the compiler operates on,
and the higher level syntax is the one that is targeted by the frontend.

Most notably, this version is not mutually recursive with the SSM monad. Fork
statements have become more flat as they only reference the function they will
fork by name, and not by definition. The rest of the syntax is quite similar.

Exports a function transpile that turns a high level representation into an
equivalent program of the low level representation. -}
module SSM.Core.LowSyntax
    ( -- * Reexports from SSM.Core.Syntax
      S.Type(..)
    , S.dereference
    , S.mkReference
    , S.isReference
    , S.SSMType(..)
    , S.Reference(..)
    , S.refType
    , S.refName
    , S.SSMExp(..)
    , S.SSMLit(..)
    , S.UnaryOpE(..)
    , S.UnaryOpR(..)
    , S.BinOp(..)
    , S.expType
    , S.Name(..)
    , S.getVarName

      -- * Things declared in this module
    , Stm(..)
    , Procedure(..)
    , Program(..)
    , transpile
    ) where

import qualified SSM.Core.Syntax as S

import qualified Data.Map as Map
import Control.Monad.State.Lazy
    ( forM, modify, runState, MonadState(put, get), State )

{- | A lower level representation of the statements that make up the body of
an SSM program. -}
data Stm
    {-| Create a new reference with the given name, which references a value of the
    given type, with the initial value specified by the expression. -}
    = NewRef S.Name S.Type S.SSMExp
    {-| Dereference an expression and put the result in a variable with the given name &
    with the given type.-}
    | GetRef S.Name S.Type S.Reference
    | SetRef S.Reference S.SSMExp  -- ^ Set the value of a reference
    {-| Set the value of a local expression specified by the name, with the given type,
    with the new value specified by the expression. -}
    | SetLocal S.Name S.Type S.SSMExp

    | If S.SSMExp [Stm] [Stm]  -- ^ Conditional execution
    | While S.SSMExp [Stm]     -- ^ Loop construct
    | Skip                     -- ^ No-op

    -- | After d r v - After d units of time the reference r should get the new value v
    | After S.SSMExp S.Reference S.SSMExp
    | Wait [S.Reference]  -- ^ Wait for any of the references to be written to
    {-| Fork procedures. The procedures are now identified by their name, and the fork
    site contains only that name and the arguments to apply the function to. -}
    | Fork [(String, [Either S.SSMExp S.Reference])]
    deriving (Show, Eq, Read)

-- | A procedure has a name, parameter names & types and a body.
data Procedure = Procedure
    { -- | Name of the procedure.
      name      :: String
      -- | Parameter names and types of the procedure.
     , arguments :: [(String, S.Type)]
      -- | Statements that make up this procedure.
    , body      :: [Stm]
    } deriving (Eq, Show, Read)

{- | A program has an entry point, arguments to that entry point and a map that maps
procedure names to their definitions. -}
data Program = Program
    { -- | Name of the procedure that is the program entrypoint.
      entry :: String
      -- | Arguments the entrypoint was applied to.
    , args :: [Either S.SSMExp S.Reference]
      -- | Map that associates procedure names with their definitions.
    , funs :: Map.Map String Procedure
    } deriving (Show, Read)


{- To get access to a program in this lower, more convenient form, we do a transpilation
pass over the high level syntax. A core purpose of this transpilation is to make the fork
statements more flat (not recursive).

When a fork statement is seen the forked procedures will contain [SSMStm]. What we do is
that we inspect the head of this list to see if the name of the procedure is one we have
already seen (is it in our map of procedures?). If we have seen it, we just refer to it
by name and move on. If we have not seen it before, we do the same things but we also
recursively transpile the forked procedure before we put it into the map of procedures. -}

-- | Transpilation monad
type Transpile a = State TranspileState a

-- | Transpilation state
data TranspileState = TranspileState
    { -- | Map that associate procedure names with their Procedure definition.
      procedures  :: Map.Map String Procedure
      -- | List of procedure names that have already been seen.
    , generated   :: [String]
      {- | Name of the procedure that was the program entrypoint?
      This is the first that that will be populated in the state during execution. -}
    , mainname    :: Maybe String
      -- | Parameter names and types of the program entrypoint.
    , mainargs    :: [(String, S.Type)]
      -- | What values were the program entrypoint applied to?
    , mainargvals :: [Either S.SSMExp S.Reference]
    }

-- | Transpile a program of high level syntax to low level syntax.
transpile :: S.SSM () -> Program
transpile program = case mainname st of

    Nothing -> error $ "no name found for the program entrypoint - did you forget to use box?"

    Just n  -> if n `elem` (Map.keys $ procedures st)
      then Program n (mainargvals st) (procedures st)
      else let p = (Procedure n (mainargs st) main)
           in Program n (mainargvals st) $ Map.insert n p (procedures st)
  where
      (main,st) = runState comp state
      state     = TranspileState Map.empty [] Nothing [] []
      comp      = transpileProcedure $ S.runSSM program

-- | Transpile a list of high level statements to a list of low level statements.
transpileProcedure :: [S.SSMStm] -> Transpile [Stm]
transpileProcedure xs = fmap concat $ forM xs $ \x -> case x of
    S.NewRef n e     -> return $ [NewRef n (S.expType e) e]
    S.GetRef n r     -> return $ [GetRef n (S.dereference (S.refType r)) r]
    S.SetRef r e     -> return $ [SetRef r e]
    S.SetLocal (S.Var t n) e2 -> return $ [SetLocal (S.Fresh n) t e2]
    S.SetLocal _ _ -> error "Trying to set a local variable that is not a variable"

    S.If c thn els -> do
      thn' <- transpileProcedure (S.runSSM thn)
      els' <- case els of
                Just els' -> transpileProcedure (S.runSSM els')
                Nothing -> return $ [Skip]
      return $ [If c thn' els']
    S.While c bdy  -> do
        bdy' <- transpileProcedure (S.runSSM bdy)
        return $ [While c bdy']
    
    S.After d r v -> return $ [After d r v]
    S.Wait refs   -> return $ [Wait refs]
    S.Fork procs  -> do
      procs' <- mapM getCall procs
      return $ [Fork procs']

    S.Procedure n    -> do
      -- Only update the mainname value if it is `Nothing`, otherwise keep
      -- the previous value.
      modify $ \st -> st { mainname = maybe (Just n) Just $ mainname st }
      return []
    S.Argument n x a -> do 
      let arginfo = (x, either S.expType S.refType a)
      let a'      = either (Left) (Right . (,) x . snd) a
      modify $ \st -> st { mainargs    = mainargs st ++ [arginfo]
                         , mainargvals = mainargvals st ++ [a']
                         }
      return []
    S.Result n       -> return []
  where
      {- | Converts a `SSM ()` computation to a description of the call, and if necessary,
      this function will also update the environment to contain a mapping from the argument
      procedure to it's body. -}
      getCall :: S.SSM () -> Transpile (String, [Either S.SSMExp S.Reference])
      getCall ssm = do
          let stmts           = S.runSSM ssm
          let name            = S.getProcedureName stmts
          let (arginfo, args) = unzip $ getArgs stmts
          st                  <- get

          if name `elem` generated st
              then return () -- we've seen it before, do nothing
              else do
                  put $ st { generated = name : generated st }
                  nstmts <- recursive $ transpileProcedure stmts
                  let fun = Procedure name arginfo nstmts
                  modify $ \st -> st { procedures = Map.insert name fun (procedures st) }
          
          return (name, args)

      {- | Perform a transpilation action without overwriting the mainname, mainargs
      & mainvals state components. Return the result of the given transpilation action -}
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
      getArgs :: [S.SSMStm] -> [((String, S.Type), Either S.SSMExp S.Reference)]
      getArgs []                    = []
      getArgs (S.Procedure _: xs)   = getArgs xs
      getArgs (S.Argument _ x a:xs) = ((x, either S.expType S.refType a), a) : getArgs xs
      getArgs _                     = []
