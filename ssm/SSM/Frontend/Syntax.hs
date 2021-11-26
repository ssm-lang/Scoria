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
{-# LANGUAGE MultiParamTypeClasses #-}
module SSM.Frontend.Syntax
  ( -- * Types
    Type(..)
  , dereference
  , mkReference
  , isReference
  , SSMType(..)

      -- * References
  , Reference(..)
  , refType
  , refName
  , renameRef
  , makeDynamicRef
  , makeStaticRef

      -- * Expressions
  , S.SSMExp(..)
  , S.SSMLit(..)
  , S.UnaryOpE(..)
  , S.UnaryOpR(..)
  , S.BinOp(..)
  , S.expType

      -- * Identifiers
  , Ident(..)

      -- * Statements
  , SSMStm(..)
  , getProcedureName
  , renameStmt
  , isHandler

      -- * SSM Monad
  , SSM(..)
  , SSMSt(..)
  , runSSM
  , emit
  , fresh

      -- * Transpilation
  , transpile
  ) where

import           SSM.Core.Ident
import qualified SSM.Core.Program              as SP
import           SSM.Core.Reference
import qualified SSM.Core.Syntax               as S
import           SSM.Core.Type
import           SSM.Core.Peripheral

import qualified Data.List                     as L
import qualified Data.Map                      as Map
import           SSM.Util.State

import           Control.Monad.State            ( MonadState
                                                , State
                                                , StateT(StateT)
                                                , execState
                                                , forM
                                                , get
                                                , gets
                                                , modify
                                                , put
                                                , runState
                                                )

{- | High level representation of the statements that can be executed in a program
of the sparse synchronous model-domain. It is very similar to the abstract synatx defined
in "SSM.Core.Syntax", but with some changes (@Fork@, @If@, @While@, most notably). -}
data SSMStm
    -- | Variable/Stream operations
    = NewRef Ident S.SSMExp       -- ^ Create a new named reference with an initial value
    | SetRef Reference S.SSMExp  -- ^ Set a reference
    {-| Set a local variable. Expression variables can currently only be created when
    they are given to a procedure as an argument, or by dereferencing a reference. -}
    | SetLocal S.SSMExp S.SSMExp

    -- | Control operations
    | If S.SSMExp (SSM ()) (Maybe (SSM ()))  -- ^ Conditional execution
    | While S.SSMExp (SSM ())                -- ^ Loop construct

    -- | SSM specific operations
    | After S.SSMExp Reference S.SSMExp  -- ^ Scheduled assignment
    | Wait [Reference]                   -- ^ Wait for any of the references to be written to
    | Fork [SSM ()]                        -- ^ Fork a list of procedures

    -- | Procedure construction
    | Procedure Ident [(Ident, Either S.SSMExp Reference)] [SSMStm]
    {-| Records the name an argument has and what value the procedure was applied to -}
    | Argument Ident Ident (Either S.SSMExp Reference)
    | Result Ident  -- ^ Mark the end of a procedure
    | Handler Handler

renameStmt :: SSMStm -> (Maybe String, Maybe (String, Int, Int)) -> SSMStm
renameStmt s (Nothing, _      ) = s
renameStmt s (_      , Nothing) = s
renameStmt s (Just n, info) =
  let srcinfo = Ident n info
  in  case s of
        NewRef n e -> NewRef srcinfo e
        _          -> s

{- | Check if an `SSM` computation represents a call to a single handler. In that case,
return the handler. Otherwise, return Nothing. -}
isHandler :: SSM () -> Maybe [Handler]
isHandler ssm = case fetchHandlers $ runSSM ssm of
  [] -> Nothing
  handlers -> Just handlers
  where
    fetchHandlers :: [SSMStm] -> [Handler]
    fetchHandlers stmts = map unwrapHandler $ filter isHandler' stmts

    isHandler' :: SSMStm -> Bool
    isHandler' (Handler _) = True
    isHandler' _           = False

    unwrapHandler :: SSMStm -> Handler
    unwrapHandler (Handler h) = h
    unwrapHandler _           = error "not a handler, robert did a mistake somewhere"

{- | The state maintained by the SSM monad. A counter for generating fresh names and
a list of statements that make up the program. -}
data SSMSt = SSMSt
  { counter    :: Int
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
  let (_, st) = runState program (SSMSt i []) in (statements st, counter st)

-- | Emit an SSM statement.
emit :: SSMStm -> SSM ()
emit stm = modify $ \st -> st { statements = statements st ++ [stm] }

{- | @IntState@ Instance for the SSM state, so that we can generate fresh names using
the generic `SSM.Util.State.Fresh` for generating fresh names. -}
instance IntState SSMSt where
  getInt = counter
  setInt i st = st { counter = i }

{- | Get the name of a procedure, where the procedure is represented by a list of
statements that make up its body. -}
getProcedureName :: [SSMStm] -> Ident
getProcedureName (Procedure n _ _ : _) = n
getProcedureName _                     = error "not a procedure"

{- | Instance of `SSM.Core.Syntax.SSMProgram`, so that the compiler knows how to turn
the frontend representation into something that it can generate code for. Just compiling
a program does not introduce any global variables. -}
instance SP.SSMProgram backend (SSM ()) where
  toProgram p =
    let (n, f) = transpile p in SP.Program [SP.SSMProcedure n []] f []

{-********** Transpiling to core syntax **********-}

-- | Transpilation monad
type Transpile a = State TranspileState a

-- | Transpilation state
data TranspileState = TranspileState
  { -- | Map that associate procedure names with their Procedure definition.
    procedures  :: Map.Map Ident SP.Procedure
      -- | List of procedure names that have already been seen.
  , generated   :: [Ident]
      {- | Last known SSM name-generating state. A SSM computation can generate names,
      but it also contains recursive SSM computations. What we want to do is essentially
      to run all of these SSM computations with a single name generating state. This
      component is that state, and we pass it in to the SSM run-function when we need
      the statements. -}
  , namecounter :: Int
  }

instance IntState TranspileState where
  getInt = namecounter
  setInt i st = st { namecounter = i }

{- | Transpile a program of the high level syntax to the low level syntax as defined
in "SSM.Core.Syntax". -}
transpile :: SSM () -> (Ident, Map.Map Ident SP.Procedure)
transpile program =
  let procs = procedures st
      funs  = if not $ name `elem` Map.keys procs
        then Map.insert name (SP.Procedure name [] main) procs
        else procs
  in  (name, funs)
 where
  (stmts@[Procedure name [] body], c ) = genStmts 0 program
  (main , st) = runState (transpileProcedure body) state
  state       = TranspileState Map.empty [] c

transpileProcedure :: [SSMStm] -> Transpile [S.Stm]
transpileProcedure xs = fmap concat $ forM xs $ \x -> case x of
  NewRef   n           e  -> return $ [S.NewRef n (S.expType e) e]
  SetRef   r           e  -> return $ [S.SetRef r e]
  SetLocal (S.Var t n) e2 -> return $ [S.SetLocal n t e2]
  SetLocal _ _ -> error "Trying to set a local variable that is not a variable"

  If c thn els            -> do
    thn' <- transpileProcedure =<< getStmts thn
    els' <- case els of
      Just els' -> transpileProcedure =<< getStmts els'
      Nothing   -> return $ [S.Skip]
    return $ [S.If c thn' els']
  While c bdy -> do
    bdy' <- transpileProcedure =<< getStmts bdy
    return $ [S.While c bdy']

  After d r v -> return $ [S.After d r v]
  Wait refs   -> return $ [S.Wait refs]
  Fork procs  -> do
    procs' <- mapM getCall procs
    return $ [S.Fork procs']

  Procedure n _ _   -> return []
  Argument n x a -> return []
  Result n       -> return []
  Handler h      -> return []
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
  getCall :: SSM () -> Transpile (Ident, [Either S.SSMExp Reference])
  getCall ssm = case runSSM ssm of
    [Procedure name nargs body] -> do
      let [Procedure name nargs body] = runSSM ssm
      let (arginfo,args) = unzip $ map (\(id,a) -> ((id, either S.expType refType a), a)) nargs
      st <- get

      if name `elem` generated st
        then return () -- we've seen it before, do nothing
        else do
          put $ st { generated = name : generated st }
          nstmts <- transpileProcedure body
          let fun = SP.Procedure name arginfo nstmts
          modify $ \st -> st { procedures = Map.insert name fun (procedures st) }

      return (name, args)

    stmts -> synthesizeProcedure stmts

---------- Synthesize procedure ----------

synthesizeProcedure :: [SSMStm] -> Transpile (Ident, [Either S.SSMExp Reference])
synthesizeProcedure body = do
  name  <- (makeIdent . (<>) "generated") <$> fresh
  stmts <- transpileProcedure body
  let toapply   = L.nub $ freeInStm [] stmts
      procedure = SP.Procedure name (map (either expInfo refInfo) toapply) stmts
  modify $ \st -> st { procedures = Map.insert name procedure (procedures st)}
  return (name, toapply)
  where
    expInfo :: S.SSMExp -> (Ident, Type)
    expInfo (S.Var t id) = (id, t)
    expInfo _ = error $ unlines [ "SSM.Frontend.Syntax.expInfo error ---"
                                , "expInfo applied to expression that is not a variable"
                                ]

    refInfo :: Reference -> (Ident, Type)
    refInfo r = (refIdent r, refType r)

freeInStm :: [Reference] -> [S.Stm] -> [Either S.SSMExp Reference]
freeInStm _ [] = []
freeInStm bound (x:xs) = case x of
  S.NewRef id t e ->
    freeInExp bound e <> freeInStm (makeDynamicRef id (mkReference t):bound) xs
  S.SetRef r e ->
    if r `elem` bound
      then freeInExp bound e <> freeInStm bound xs
      else Right r : freeInExp bound e <> freeInStm bound xs
  S.SetLocal id t e ->
    Left (S.Var t id) : freeInExp bound e <> freeInStm bound xs
  S.If c thn els ->
    let inc = freeInExp bound c
        inthn = freeInStm bound thn
        inels = freeInStm bound els
    in inc <> inthn <> inels <> freeInStm bound xs
  S.While c bdy ->
    let inc = freeInExp bound c
        inbdy = freeInStm bound bdy
    in inc <> inbdy <> freeInStm bound xs
  S.Skip -> freeInStm bound xs
  S.After e r v ->
    let ine = freeInExp bound e
        inv = freeInExp bound v
    in if r `elem` bound
      then ine <> inv <> freeInStm bound xs
      else Right r : ine <> inv <> freeInStm bound xs
  S.Wait refs ->
    let inrefs = filter (not . flip elem bound) refs
    in map Right inrefs <> freeInStm bound xs
  S.Fork cs ->
    let args = concatMap snd cs
        keepExp e = if S.isVar e then [Left e] else []
        keepRef r = if r `elem` bound then [] else [Right r]
    in concatMap (either keepExp keepRef) args

freeInExp :: [Reference] -> S.SSMExp -> [Either S.SSMExp Reference]
freeInExp bound e = case e of
  S.Var _ id      -> [Left e]
  S.Lit _ _       -> []
  S.UOpE _ e _    -> freeInExp bound e
  S.UOpR _ r _    -> if r `elem` bound then [] else [Right r]
  S.BOp _ e1 e2 _ -> freeInExp bound e1 <> freeInExp bound e2
