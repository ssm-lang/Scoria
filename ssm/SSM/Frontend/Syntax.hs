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

      -- * Names
    , S.Name(..)
    , S.getVarName

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

import Control.Monad.State
    ( gets, get, put, modify, execState, runState, forM, MonadState, State, StateT(StateT) )

{- | High level representation of the statements that can be executed in a program
of the sparse synchronous model-domain. It is very similar to the abstract synatx defined
in "SSM.Core.Syntax", but with some changes (@Fork@, @If@, @While@, most notably). -}
data SSMStm
    -- | Variable/Stream operations
    = NewRef S.Name S.SSMExp       -- ^ Create a new named reference with an initial value
    | GetRef S.Name S.Reference    -- ^ Dereference a reference, place the result in a var
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
    | Procedure String  -- ^ Marks the start of a procedure
    {-| Records the name an argument has and what value the procedure was applied to -}
    | Argument String String (Either S.SSMExp S.Reference)
    | Result String  -- ^ Mark the end of a procedure

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
getProcedureName :: [SSMStm] -> String
getProcedureName (Procedure n:_) = n
getProcedureName _               = error "not a procedure"

{- To get access to a program in this lower, more convenient form, we do a transpilation
pass over the high level syntax. A core purpose of this transpilation is to make the fork
statements more flat (not recursive).

When a fork statement is seen the forked procedures will contain @[SSMStm]@. What we do is
that we inspect the head of this list to see if the name of the procedure is one we have
already seen (is it in our map of procedures?). If we have seen it, we just refer to it
by name and move on. If we have not seen it before, we do the same things but we also
recursively transpile the forked procedure before we put it into the map of procedures. -}

-- | Transpilation monad
type Transpile a = State TranspileState a

-- | Transpilation state
data TranspileState = TranspileState
    { -- | Map that associate procedure names with their Procedure definition.
      procedures  :: Map.Map String S.Procedure
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

{- | Transpile a program of the high level syntax to the low level syntax as defined
in "SSM.Core.Syntax". -}
transpile :: SSM ()
          -> ( String                         -- Entry-point of the program
             , [Either S.SSMExp S.Reference]  -- Arguments to apply it to
             , Map.Map String S.Procedure     -- Rest of procedures in the program
             )
transpile program = case mainname st of

    Nothing -> error $ "no name found for the program entrypoint - did you forget to use box?"

    Just n  -> if n `elem` (Map.keys $ procedures st)
      then (n, mainargvals st, procedures st)
      else let p = (S.Procedure n (mainargs st) main)
           in (n, mainargvals st, Map.insert n p (procedures st))
  where
      (main,st) = runState comp state
      state     = TranspileState Map.empty [] Nothing [] []
      comp      = transpileProcedure $ runSSM program

-- | Transpile a list of high level statements to a list of low level statements.
transpileProcedure :: [SSMStm] -> Transpile [S.Stm]
transpileProcedure xs = fmap concat $ forM xs $ \x -> case x of
    NewRef n e     -> return $ [S.NewRef n (S.expType e) e]
    GetRef n r     -> return $ [S.GetRef n (S.dereference (S.refType r)) r]
    SetRef r e     -> return $ [S.SetRef r e]
    SetLocal (S.Var t n) e2 -> return $ [S.SetLocal (S.Fresh n) t e2]
    SetLocal _ _ -> error "Trying to set a local variable that is not a variable"

    If c thn els -> do
      thn' <- transpileProcedure (runSSM thn)
      els' <- case els of
                Just els' -> transpileProcedure (runSSM els')
                Nothing -> return $ [S.Skip]
      return $ [S.If c thn' els']
    While c bdy  -> do
        bdy' <- transpileProcedure (runSSM bdy)
        return $ [S.While c bdy']
    
    After d r v -> return $ [S.After d r v]
    Wait refs   -> return $ [S.Wait refs]
    Fork procs  -> do
      procs' <- mapM getCall procs
      return $ [S.Fork procs']

    Procedure n    -> do
      -- Only update the mainname value if it is `Nothing`, otherwise keep
      -- the previous value.
      modify $ \st -> st { mainname = maybe (Just n) Just $ mainname st }
      return []
    Argument n x a -> do 
      let arginfo = (x, either S.expType S.refType a)
      let a'      = either Left (Right . flip S.renameRef x) a
      modify $ \st -> st { mainargs    = mainargs st ++ [arginfo]
                         , mainargvals = mainargvals st ++ [a']
                         }
      return []
    Result n       -> return []
  where
      {- | Converts a `SSM ()` computation to a description of the call, and if necessary,
      this function will also update the environment to contain a mapping from the argument
      procedure to it's body. -}
      getCall :: SSM () -> Transpile (String, [Either S.SSMExp S.Reference])
      getCall ssm = do
          let stmts           = runSSM ssm
          let name            = getProcedureName stmts
          let (arginfo, args) = unzip $ getArgs stmts
          st                  <- get

          if name `elem` generated st
              then return () -- we've seen it before, do nothing
              else do
                  put $ st { generated = name : generated st }
                  nstmts <- recursive $ transpileProcedure stmts
                  let fun = S.Procedure name arginfo nstmts
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
      getArgs :: [SSMStm] -> [((String, S.Type), Either S.SSMExp S.Reference)]
      getArgs []                    = []
      getArgs (Procedure _: xs)   = getArgs xs
      getArgs (Argument _ x a:xs) = ((x, either S.expType S.refType a), a) : getArgs xs
      getArgs _                     = []

{- | Instance of `SSM.Core.Syntax.SSMProgram`, so that the compiler knows how to turn
the frontend representation into something that it can generate code for. Just compiling
a program does not introduce any global variables. -}
instance S.SSMProgram (SSM ()) where
  toProgram p = let (n,a,f) = transpile p
                in S.Program n a f []

-- compile monad

data CompileSt = CompileSt
    { compileCounter   :: Int
    , generatedGlobals :: [(String, S.Type)]
    }

-- | Compile monad used to set up global variables before running a program
newtype Compile a = Compile (State CompileSt a)
  deriving Functor                via State CompileSt
  deriving Applicative            via State CompileSt
  deriving Monad                  via State CompileSt
  deriving (MonadState CompileSt) via State CompileSt

{- | @IntState@ instance for `CompileSt` so that the `Compile` monad can generate
fresh names. -}
instance IntState CompileSt where
  getInt = compileCounter
  setInt i st = st { compileCounter = i }

-- | Add the name and type of a global variable to the Compile monad
addGlobal :: String -> S.Type -> Compile ()
addGlobal name t = do
  s <- get
  if name `elem` map fst (generatedGlobals s)
    then error $
      concat ["name ", name, " has already been declared as a global variable"]
    else modify $ \st ->
      st { generatedGlobals = generatedGlobals st ++ [(name, t)]}

{- | Meant to be used in infix position, like @st1 `hasSameGlobalsAs` st2@. Returns
@True@ if, as the same name, the two compile states has the same global references
declared. -}
hasSameGlobalsAs :: CompileSt -> CompileSt -> Bool
hasSameGlobalsAs st1 st2 = generatedGlobals st1 == generatedGlobals st2

{- | Rename the newst declared global reference by giving it the name that is supplied
to this function.

NOTE: This function is only meant to be called by the BinderAdd library. -}
renameNewestGlobal :: String -> Compile ()
renameNewestGlobal name = do
  st <- get
  let newest = last (generatedGlobals st)
  modify $ \st ->
    st { generatedGlobals = init (generatedGlobals st) ++ [(name, snd newest)] }

{- | If you have a @Compile (SSM ())@ you have probably set up some global variables
using the @Compile@ monad. This instance makes sure that you can something that is a
program with these global variables. -}
instance S.SSMProgram (Compile (SSM ())) where
  toProgram (Compile p) = let (a,s) = runState p (CompileSt 0 [])
                              (n,ar,f) = transpile a
                          in S.Program n ar f $ generatedGlobals s
