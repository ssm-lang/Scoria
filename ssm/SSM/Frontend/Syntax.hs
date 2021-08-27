{-| This module exports the abstract syntax of the embedded language. The
syntax is at a higher level than that of "SSM.Core.Syntax", and must be
transpiled to the lower representation before it can be compiled or interpreted.

A lot of the types and functions that are exported are simply re-exported from
"SSM.Core.Syntax". Please refer to that module for more detailed documentation, if
not a lot can be found in this module. The most notable change is that these
high level statements are mutually recursively defined with a monad that is
used to construct programs. Another notable change is that fork statements now
contain not just a description of the forked call, but rather the complete
procedure definition (as a monadic computation).
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
module SSM.Frontend.Syntax
    ( -- * Types
      S.Type(..)
    , S.dereference
    , S.mkReference
    , S.isReference
    , S.SSMType(..)

      -- * References
    , S.Reference(..)
    , S.refType
    , S.refName
    , S.renameRef
    , S.makeDynamicRef
    , S.makeStaticRef

      -- * Expressions
    , S.SSMExp(..)
    , S.SSMLit(..)
    , S.UnaryOpE(..)
    , S.UnaryOpR(..)
    , S.BinOp(..)
    , S.expType

      -- * Time
    , S.SSMTimeUnit(..)
    , S.SSMTime(..)

      -- * Identifiers
    , S.Ident(..)

      -- * Statements
    , SSMStm(..)
    , getProcedureName
    , renameStmt

      -- * SSM Monad
    , SSM(..)
    , SSMSt(..)
    , runSSM
    , pureSSM
    , emit
    , fresh

      -- * Transpilation
    , transpile

) where

import SSM.Util.State
import qualified SSM.Core.Syntax as S

import qualified Data.Map as Map

import Control.Monad.State
    ( gets, get, put, modify, execState, runState, forM, MonadState, State, StateT(StateT) )
import qualified System.Console.GetOpt as S

{- | High level representation of the statements that can be executed in a program
of the sparse synchronous model-domain. It is very similar to the abstract synatx defined
in "SSM.Core.Syntax", but with some changes (@Fork@, @If@, @While@, most notably). -}
data SSMStm
    -- | Variable/Stream operations
    = NewRef S.Ident S.SSMExp       -- ^ Create a new named reference with an initial value
    | SetRef S.Reference S.SSMExp  -- ^ Set a reference
    {-| Set a local variable. Expression variables can currently only be created when
    they are given to a procedure as an argument, or by dereferencing a reference. -}
    | SetLocal S.SSMExp S.SSMExp

    -- | Control operations
    | If S.SSMExp (SSM ()) (Maybe (SSM ()))  -- ^ Conditional execution
    | While S.SSMExp (SSM ())                -- ^ Loop construct
            
    -- | SSM specific operations
    | After S.SSMTime S.Reference S.SSMExp  -- ^ Scheduled assignment
    | Wait [S.Reference]                   -- ^ Wait for any of the references to be written to
    | Fork [SSM ()]                        -- ^ Fork a list of procedures

    -- | Procedure construction
    | Procedure S.Ident  -- ^ Marks the start of a procedure
    {-| Records the name an argument has and what value the procedure was applied to -}
    | Argument S.Ident S.Ident (Either S.SSMExp S.Reference)
    | Result S.Ident  -- ^ Mark the end of a procedure

renameStmt :: SSMStm -> (Maybe String, Maybe (String, Int, Int)) -> SSMStm
renameStmt s (Nothing, _)               = s
renameStmt s (_, Nothing)               = s
renameStmt s (Just n, info) =
    let srcinfo = S.Ident n info
    in case s of
        NewRef n e  -> NewRef srcinfo e
        _           -> s

{- | The state maintained by the SSM monad. A counter for generating fresh names and
a list of statements that make up the program. -}
data SSMSt = SSMSt { counter    :: Int
                   , statements :: [SSMStm]
                   }

-- | The SSM monad is used to build programs.
newtype SSM a = SSM (State SSMSt a)
  deriving Functor            via State SSMSt
  deriving Applicative        via State SSMSt
  deriving Monad              via State SSMSt
  deriving (MonadState SSMSt) via State SSMSt

-- | Run a SSM program and get the statements from it.
runSSM :: SSM a -> [SSMStm]
runSSM (SSM program) = statements $ execState program (SSMSt 0 [])

{- | Run a SSM program with an initial state and get the statements as well as the
final state. -}
genStmts :: Int -> SSM a -> ([SSMStm], Int)
genStmts i (SSM program) =
  let (_,st) = runState program (SSMSt i [])
  in (statements st, counter st)

-- | Take a list of statements and turn them into an SSM computation.
pureSSM :: [SSMStm] -> SSM ()
pureSSM stmts = modify $ \st -> st { statements = stmts }

-- | Emit an SSM statement.
emit :: SSMStm -> SSM ()
emit stm = modify $ \st -> st { statements = statements st ++ [stm]}

{- | @IntState@ Instance for the SSM state, so that we can generate fresh names using
the generic `SSM.Util.State.Fresh` for generating fresh names. -}
instance IntState SSMSt where
  getInt = counter
  setInt i st = st { counter = i }

{- | Get the name of a procedure, where the procedure is represented by a list of
statements that make up its body. -}
getProcedureName :: [SSMStm] -> S.Ident
getProcedureName (Procedure n:_) = n
getProcedureName _               = error "not a procedure"

{- | Instance of `SSM.Core.Syntax.SSMProgram`, so that the compiler knows how to turn
the frontend representation into something that it can generate code for. Just compiling
a program does not introduce any global variables. -}
instance S.SSMProgram (SSM ()) where
  toProgram p = let (n,f) = transpile p
                in S.Program [S.SSMProcedure n []] f [] Nothing Nothing

{-********** Transpiling to core syntax **********-}

-- | Transpilation monad
type Transpile a = State TranspileState a

-- | Transpilation state
data TranspileState = TranspileState
    { -- | Map that associate procedure names with their Procedure definition.
      procedures  :: Map.Map S.Ident S.Procedure
      -- | List of procedure names that have already been seen.
    , generated   :: [S.Ident]
      {- | Last known SSM name-generating state. A SSM computation can generate names,
      but it also contains recursive SSM computations. What we want to do is essentially
      to run all of these SSM computations with a single name generating state. This
      component is that state, and we pass it in to the SSM run-function when we need
      the statements. -}
    , namecounter :: Int
    }

{- | Transpile a program of the high level syntax to the low level syntax as defined
in "SSM.Core.Syntax". -}
transpile :: SSM () -> (S.Ident, Map.Map S.Ident S.Procedure)
transpile program =
  let n     = getProcedureName stmts
      procs = procedures st
      {- A procedure is only inserted in the resulting map if it was reached
      through a fork. If the entry point was not accessed by another procedure by a
      fork, we need to manually insert it in the map. -}
      funs  = if not $ n `elem` Map.keys procs
                then Map.insert n (S.Procedure n [] main) procs
                else procs
  in (n, funs)
  where
      (main,st)  = runState comp state
      (stmts, c) = genStmts 0 program
      state      = TranspileState Map.empty [] c
      comp       = transpileProcedure stmts


transpileProcedure :: [SSMStm] -> Transpile [S.Stm]
transpileProcedure xs = fmap concat $ forM xs $ \x -> case x of
    NewRef n e     -> return $ [S.NewRef n (S.expType e) e]
    SetRef r e     -> return $ [S.SetRef r e]
    SetLocal (S.Var t n) e2 -> return $ [S.SetLocal n t e2]
    SetLocal _ _ -> error "Trying to set a local variable that is not a variable"

    If c thn els -> do
      thn' <- transpileProcedure =<< getStmts thn
      els' <- case els of
                Just els' -> transpileProcedure =<< getStmts els'
                Nothing -> return $ [S.Skip]
      return $ [S.If c thn' els']
    While c bdy  -> do
        bdy' <- transpileProcedure =<< getStmts bdy
        return $ [S.While c bdy']
    
    After d r v -> return $ [S.After d r v]
    Wait refs   -> return $ [S.Wait refs]
    Fork procs  -> do
      procs' <- mapM getCall procs
      return $ [S.Fork procs']

    Procedure n    -> return []
    Argument n x a -> return []
    Result n       -> return []
  where
      {- | Run a recursive SSM computation by using the last known name generating state.
      The last known name-generating state is updated to reflect if any new names were
      generated while running this computation. -}
      getStmts :: SSM () -> Transpile [SSMStm]
      getStmts p = do
        counter <- gets namecounter
        let (stmts, counter') = genStmts counter p
        modify $ \st -> st { namecounter = counter' }
        return stmts

      {- | Converts a `SSM ()` computation to a description of the call, and if necessary,
      this function will also update the environment to contain a mapping from the argument
      procedure to it's body. -}
      getCall :: SSM () -> Transpile (S.Ident, [Either S.SSMExp S.Reference])
      getCall ssm = do
          let stmts           = runSSM ssm
          let name            = getProcedureName stmts
          let (arginfo, args) = unzip $ getArgs stmts
          st                  <- get

          if name `elem` generated st
              then return () -- we've seen it before, do nothing
              else do
                  put $ st { generated = name : generated st }
                  nstmts <- transpileProcedure stmts
                  let fun = S.Procedure name arginfo nstmts
                  modify $ \st -> st { procedures = Map.insert name fun (procedures st) }
          
          return (name, args)

      {-| Return a tuple where the first component contains information about name
      and type about the arguments, and the second compoment is a list of the actual arguments. -}
      getArgs :: [SSMStm] -> [((S.Ident, S.Type), Either S.SSMExp S.Reference)]
      getArgs []                    = []
      getArgs (Procedure _: xs)     = getArgs xs
      getArgs (Argument _ x a:xs)   = ((x, either S.expType S.refType a), a) : getArgs xs
      getArgs _                     = []
