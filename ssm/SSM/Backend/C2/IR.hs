{- | This module exposes an IR that is lower than the one found in @SSM.Core.Program@,
and it -}
module SSM.Backend.C2.IR where

import qualified SSM.Core as S

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader

import qualified Data.Map as Map

import Data.Bifunctor
import Data.Either
import Data.List

data MUExp
    = Marshal MUExp
    | Unmarshal MUExp
    | Var S.Type S.Ident
    | Lit S.Type S.SSMLit
    | UOpE S.Type MUExp S.UnaryOpE
    | UOpR S.Type S.Reference S.UnaryOpR
    | BOp S.Type MUExp MUExp S.BinOp
  deriving Show

ssmexpToMUExp :: S.SSMExp -> MUExp
ssmexpToMUExp e = case e of
    S.Var t n         -> Var t n
    S.Lit t l         -> Marshal $ Lit t l
    S.UOpE t e op     -> Marshal $ UOpE t (unmarshal $ ssmexpToMUExp e) op
    S.UOpR t r op     -> Marshal $ UOpR t r op
    S.BOp t e1 e2 bop -> Marshal $ BOp t (unmarshal $ ssmexpToMUExp e1) (unmarshal $ ssmexpToMUExp e2) bop

unmarshal :: MUExp -> MUExp
unmarshal (Marshal e) = e
unmarshal e           = Unmarshal e

testE :: S.SSMExp
testE = S.BOp S.TInt32 (S.Lit S.TInt32 (S.LInt32 5)) (S.Lit S.TInt32 (S.LInt32 5)) S.OPlus

lowerexp :: S.SSMExp -> MUExp
lowerexp = ssmexpToMUExp

data Stm =
    -- Reference management
    CreateRef Int S.Ident S.Type
  | InitializeRef Int S.Reference MUExp
  | SetRef Int S.Reference MUExp

    -- Control flow
  | If Int MUExp [Stm] [Stm]
  | While Int MUExp [Stm]
  | Skip Int

    -- Time management
  | After Int MUExp S.Reference MUExp
  | Sensitize Int S.Reference
  | Desensitize Int S.Reference

    -- Procedure management
  | Fork Int S.Ident Int Int [Either MUExp S.Reference]
  | Yield Int
  | Terminate Int

    -- Memory management
  | Dup Int S.Reference
  | Drop Int S.Reference

    -- State management
  | SetState Int Int
  deriving Show

data Procedure = Procedure
  { name      :: S.Ident
  , arguments :: [(S.Ident, S.Type)]
  , body      :: [Stm]
  }
  deriving Show

data QueueContent backend
    = SSMProcedure S.Ident [Either MUExp S.Reference]
    | OutputHandler (S.Handler backend)

instance Show (QueueContent backend) where
  show (SSMProcedure id args) = "SSMProcedure " <> show id <> " " <> show args
  show (OutputHandler _)      = "<output-handler>"

data Program backend = Program
  { initialQueueContent :: [QueueContent backend]
  , funs                :: Map.Map S.Ident Procedure
  , peripherals         :: [S.Peripheral backend]
  }
  deriving (Show)

-- Pretty

type Pretty a = WriterT [String] (Reader Int) a

indent :: Pretty a -> Pretty a
indent pa = local (+2) pa

emit :: String -> Pretty ()
emit str = do
    ind <- ask
    tell [replicate ind ' ' <> str]

prettyProgram :: Program backend -> String
prettyProgram p =
    let x = runReader (execWriterT (mapM_ prettyProcedure $ Map.elems (funs p))) 0
    in unlines x

prettyProcedure :: Procedure -> Pretty ()
prettyProcedure p = do
    emit $ concat [S.identName $ name p, "(", prettyArgs $ arguments p, ")"]
    mapM_ (indent . prettyStm) (body p)
  where
      prettyStm :: Stm -> Pretty ()
      prettyStm stm = case stm of
        CreateRef n id t -> emit $
            concat [show n, ": ", prettyType t, " ", S.identName id]
        InitializeRef n r e -> emit $
            concat [show n, ": initialize(", S.refName r,", ", prettyExp e, ")"]
        SetRef n r e -> emit $
            concat [show n, ": ", S.refName r, " = ", prettyExp e]
        If n c thn els -> do
            emit $ concat [show n, ": if(", prettyExp c, ")"]
            indent $ mapM_ prettyStm thn
            emit "else"
            indent $ mapM_ prettyStm els
        While n c bdy -> do
            emit $ concat [show n, ": while(", prettyExp c, ")"]
            indent $ mapM_ prettyStm bdy
        Skip n -> emit $ concat [show n, ": skip"]
        After n d r v -> emit $
            concat [show n, ": after (", prettyExp d, ") ", S.refName r, " = ", prettyExp v]
        Sensitize n r -> emit $ concat [show n, ": sensitize ", S.refName r]
        Desensitize n r -> emit $ concat [show n, ": desensitize ", S.refName r]
        Fork n id k i args -> emit $
            concat [show n, ": fork-", S.identName id, "(", intercalate ", " (map (either prettyExp S.refName) args), ")"]
        Yield n -> emit $ concat [show n, ": yield"]
        Terminate n -> emit $ concat [show n, ": terminate"]
        Dup n r -> emit $ concat [show n, ": dup ", S.refName r]
        Drop n r -> emit $ concat [show n, ": drop ", S.refName r]
        SetState n st -> emit $ concat [show n, ": set-state ", show st]


      prettyArgs :: [(S.Ident, S.Type)] -> String
      prettyArgs args =
          intercalate ", " $ map (\(id,t) -> concat [prettyType t, " ", S.identName id]) args

      prettyType :: S.Type -> String
      prettyType t = case t of
          S.TUInt8  -> "u8"
          S.TUInt32 -> "u32"
          S.TUInt64 -> "u64"
          S.TInt32  -> "i32"
          S.TInt64  -> "i64"
          S.TBool   -> "bool"
          S.TEvent  -> "event"
          S.Ref t   -> prettyType t <> "*"

prettyLit :: S.SSMLit -> String
prettyLit l = case l of
    S.LInt32 i  -> show i
    S.LUInt8 i  -> show i
    S.LUInt32 i -> show i
    S.LInt64 i  -> show i
    S.LUInt64 i -> show i
    S.LBool b   -> show b
    S.LEvent    -> show ()

prettyExp :: MUExp -> String
prettyExp e = case e of
    Marshal e       -> prettyExp e
    Unmarshal e     -> prettyExp e
    Var t n         -> S.identName n
    Lit t l         -> prettyLit l
    UOpE t e op     -> prettyUnaryOpE op e
    UOpR t r op     -> prettyUnaryOpR op r
    BOp t e1 e2 bop -> concat [ "("
                              , prettyExp e1
                              , " "
                              , prettyBinop bop
                              , " "
                              , prettyExp e2
                              , ")"
                              ]

prettyUnaryOpE :: S.UnaryOpE -> MUExp -> String
prettyUnaryOpE op e = case op of
    S.Neg -> concat ["(-", prettyExp e, ")"] 
    S.Not -> concat ["!", prettyExp e]

prettyUnaryOpR :: S.UnaryOpR -> S.Reference -> String
prettyUnaryOpR op r = case op of
    S.Changed -> '@' : S.refName r
    S.Deref   -> '*' : S.refName r

prettyBinop :: S.BinOp -> String
prettyBinop op = case op of
    S.OPlus   -> "+"
    S.OMinus  -> "-"
    S.OTimes  -> "*"
    S.ODiv    -> "/"
    S.ORem    -> "%"
    S.OMin    -> "`min`"
    S.OMax    -> "`max`"
    S.OLT     -> "<"
    S.OEQ     -> "=="
    S.OAnd    -> "&&"
    S.OOr     -> "||"
    S.OLShift -> "<<"
    S.ORShift -> ">>"
    S.OBAnd   -> "&"
    S.OBOr    -> "|"
    S.OBXor   -> "xor"


-- Transpile

data TranspileSt = St
  { counter   :: Int
  , localrefs :: [S.Reference]
  , currState :: Int
  }

type Transpile a = State TranspileSt a

number :: Transpile Int
number = do
    i <- gets counter
    modify $ \st -> st { counter = counter st + 1 }
    return i

declareRef :: S.Ident -> S.Type -> Transpile S.Reference
declareRef id t = do
    let ref = S.makeDynamicRef id t
    modify $ \st -> st { localrefs = ref : localrefs st }
    return ref

incrementState :: Transpile Stm
incrementState = do
    st <- get
    put $ st { currState = currState st + 1}
    n <- number
    return $ SetState n (currState st + 1)

transpile :: S.Program backend -> Program backend
transpile p = 
    let procedures = Map.map (flip evalState (St 0 [] 0) . transpileProcedure) (S.funs p)
        initconts  = flip map (S.initialQueueContent p) $ \qc -> case qc of
            S.SSMProcedure id' args -> SSMProcedure id' $ map (bimap lowerexp id) args
            S.OutputHandler h      -> OutputHandler h
    in Program initconts procedures (S.peripherals p)

transpileProcedure :: S.Procedure -> Transpile Procedure
transpileProcedure p = do
    setstate <- SetState <$> number <*> pure 0
    body' <- transpileBody $ S.body p
    
    st <- get
    drops <- forM (localrefs st) $ \r -> do
        n <- number
        return $ Drop n r
    
    n <- number
    return $ Procedure (S.name p) (S.arguments p) (setstate : body' <> drops <> [Terminate n])
  where
      transpileBody :: [S.Stm] -> Transpile [Stm]
      transpileBody stms = flip concatMapM stms $ \stm -> case stm of
          S.NewRef id t e -> do
              n1 <- number
              n2 <- number
              ref <- declareRef id t
              return [CreateRef n1 id t, InitializeRef n2 ref (lowerexp e)]
          S.SetRef r e -> do
              n <- number
              return [SetRef n r (lowerexp e)]
          S.If c thn els -> do
              n <- number
              thn' <- transpileBody thn
              els' <- transpileBody els
              return [If n (lowerexp c) thn' els']
          S.While c bdy -> do
              n <- number
              bdy' <- transpileBody bdy
              return [While n (lowerexp c) bdy']
          S.Skip -> do
              n <- number
              return [Skip n]
          S.After e r v -> do
              n <- number
              return [After n (lowerexp e) r (lowerexp v)]
          S.Wait refs -> do
              sensitizes <- forM refs $ \r -> do
                  n <- number
                  return $ Sensitize n r
              incst <- incrementState
              n <- number
              desensitizes <- forM refs $ \r -> do
                  n <- number
                  return $ Desensitize n r
              return $ sensitizes <> [incst, Yield n] <> desensitizes
          S.Fork procs -> do
              (forks, dups) <- unzip <$> zipWithM (lowerFork $ length procs) [0..] procs
              forks' <- forM forks $ \f -> do
                  n <- number
                  return $ f n
              
              incst <- incrementState
              yield <- Yield <$> number
              
              drops <- forM (concat dups) $ \(Dup _ r) -> do
                  n <- number
                  return $ Drop n r
              
              return $ concat dups -- first dup references
                    <> forks'      -- fork the procedures
                    <> [incst, yield]     -- yield control to children
                    <> drops       -- drop reference count

      lowerFork :: Int
                -> Int
                -> (S.Ident, [Either S.SSMExp S.Reference])
                -> Transpile (Int -> Stm, [Stm])
      lowerFork k i (f, args) = do
          let toDup = map unsafeFromRight $ filter isRight args
          dups' <- mapM (\r -> Dup <$> number <*> pure r) toDup

          let args' = map (bimap lowerexp id) args

          return (\n -> Fork n f k i args', dups')
        where
            unsafeFromRight :: Either a b -> b
            unsafeFromRight (Right x) = x


concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat <$> sequence (map f xs)
