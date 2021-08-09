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

      -- * Identifiers
    , S.Ident(..)

      -- * Statements
    , SSMStm(..)
    , getProcedureName

      -- * SSM Monad
    , SSM(..)
    , SSMSt(..)
    , runSSM
    , pureSSM
    , emit
    , fresh

      -- * Transpilation
    , transpile

    , Compile
    , addGlobal
    , hasSameGlobalsAs
    , renameNewestGlobal
) where

import SSM.Util.State
import qualified SSM.Core.Syntax as S

import qualified Data.Map as Map

import Debug.Trace

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
    | After S.SSMExp S.Reference S.SSMExp  -- ^ Scheduled assignment
    | Wait [S.Reference]                   -- ^ Wait for any of the references to be written to
    | Fork [SSM ()]                        -- ^ Fork a list of procedures

    -- | Procedure construction
    | Procedure S.Ident  -- ^ Marks the start of a procedure
    {-| Records the name an argument has and what value the procedure was applied to -}
    | Argument S.Ident S.Ident (Either S.SSMExp S.Reference)
    | Result S.Ident  -- ^ Mark the end of a procedure

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
                in S.Program n f []

-- compile monad

-- | State maintained by the `Compile` monad
data CompileSt = CompileSt
    { compileCounter   :: Int                  -- ^ Counter to generate fresh named
    , generatedGlobals :: [(S.Ident, S.Type)]  -- ^ Names and types of global references
    }

-- | Compile monad used to set up global variables before running a program
newtype Compile a = Compile (State CompileSt a)
  deriving Functor                via State CompileSt
  deriving Applicative            via State CompileSt
  deriving Monad                  via State CompileSt
  deriving (MonadState CompileSt) via State CompileSt

{- | @IntState@ instance for `CompileSt` so that the `Compile` monad can generate
fresh names with the generic `SSM.Util.State.fresh` function.. -}
instance IntState CompileSt where
  getInt = compileCounter
  setInt i st = st { compileCounter = i }

-- | Add the name and type of a global variable to the Compile monad
addGlobal :: S.Ident -> S.Type -> Compile ()
addGlobal name t = do
  s <- get
  if name `elem` map fst (generatedGlobals s)
    then error $
      concat ["name ", S.identName name, " has already been declared as a global variable"]
    else modify $ \st ->
      st { generatedGlobals = generatedGlobals st ++ [(name, t)]}

{- | Meant to be used in infix position, like @st1 `hasSameGlobalsAs` st2@. Returns
@True@ if, as the name suggests, the two compile states has the same global references
declared. -}
hasSameGlobalsAs :: CompileSt -> CompileSt -> Bool
hasSameGlobalsAs st1 st2 = generatedGlobals st1 == generatedGlobals st2

{- | Rename the newst declared global reference by giving it the name that is supplied
to this function.

NOTE: This function is only meant to be called by the BinderAnn library. -}
renameNewestGlobal :: S.Ident -> Compile ()
renameNewestGlobal name = do
  st <- get
  let newest = last (generatedGlobals st)
  modify $ \st ->
    st { generatedGlobals = init (generatedGlobals st) ++ [(name, snd newest)] }

{- | If you have a @Compile (SSM ())@ you have probably set up some global variables
using the @Compile@ monad. This instance makes sure that you can compile and interpret
something that is a program with such global variables. -}
instance S.SSMProgram (Compile (SSM ())) where
  toProgram (Compile p) = let (a,s) = runState p (CompileSt 0 [])
                              (n,f) = transpile a
                          in S.Program n f $ generatedGlobals s

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
