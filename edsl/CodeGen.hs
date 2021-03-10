module CodeGen where

import Control.Monad.State
import Control.Monad.Writer
import GHC.Float
import Data.List

import Core

data CGenSt = CGenSt { widestWait  :: Int
                     , nextCase    :: Int
                     , localVars   :: [(String, Type)]
                     , nameGen     :: Int
                     }
type CGen a = StateT CGenSt -- widest wait encountered so far (determines the number of triggers) 
                (WriterT [String] 
                  IO) a

runCGen :: SSM () -> IO String
runCGen ssm = do
    let rw = evalStateT (genProcedure ssm) (CGenSt 0 0 [] 0)
    out <- execWriterT rw
    return $ unlines out

genProcedure :: SSM () -> CGen ()
genProcedure ssm = do
    struct <- censor (const []) $ snd <$> listen (genStruct ssm)
    enter  <- censor (const []) $ snd <$> listen (genEnter ssm)
    step   <- censor (const []) $ snd <$> listen (genStep ssm)
    tell [unlines struct]
    tell [""]
    tell [unlines enter]
    tell [""]
    tell [unlines step]

genStruct :: SSM () -> CGen ()
genStruct (Procedure name _ k) = do
    tell ["typedef struct {"]
    indent "/* Generic procedure fields */"
    indent "void (*step)(rar_t *); // Pointer to step function"
    indent "uint16_t pc;           // Saved control state"
    indent "rar_t *caller;         // Caller's activation record"
    indent "uint16_t children;     // Number of running children"
    indent "uint32_t priority;     // Order in the ready queue"
    indent "uint8_t depth;         // Index of LSB of our priority"
    indent "bool scheduled;        // True when in the ready queue"
    indent "/* procedure specific fields */"
    specifics $ k ()
    triggers
    tell ["} rar_" ++ name ++ "_t;"]
  where
      indent :: String -> CGen ()
      indent str = tell [replicate 4 ' ' ++ str]

      specifics :: SSM () -> CGen ()
      specifics ssm = case ssm of
          (Return x)        -> return ()
          (NewRef (Just (_,n)) e k) -> do let t = Ref $ expType e
                                          indent $ compileType t ++ n ++ ";"
                                          modify $ \st -> st { localVars = localVars st ++ [(n, t)]}
                                          specifics (k (n, t))
          (SetRef r e k)    -> specifics (k ())
          (SetLocal e v k)  -> specifics (k ())
          (GetRef (r, t) (Just (_,n)) k) -> do let t' = dereference t
                                               indent $ compileType t' ++ " " ++ n ++ ";"
                                               modify $ \st -> st { localVars = localVars st ++ [(n,t')]}
                                               specifics (k (Var t' n))
          (If c thn (Just els) k) -> specifics (thn >> els >> k ())
          (If c thn Nothing k)    -> specifics (thn >> k ())
          (While c bdy k)   -> specifics (bdy >> k ())
          (After e r v k)   -> specifics (k ())
          (Changed r (Just (_,n)) k) -> do indent $ compileType TBool ++ " " ++ n ++ ";"
                                           modify $ \st -> st { localVars = localVars st ++ [(n, TBool)]}
                                           specifics (k (Var TBool n))
          (Wait vars k)     -> do modify $ \st -> st { widestWait = max (widestWait st) (length vars)}
                                  specifics (k ())
          (Fork procs k)    -> specifics (k ())
          (Procedure n _ k) -> specifics (k ())
          (Argument n name (Left e) k)  -> do indent $ compileType (expType e) ++ " " ++ name ++ ";"
                                              specifics (k ())
          (Argument n name (Right r) k) -> do indent $ compileType (snd r) ++ name ++ ";"
                                              specifics (k ())
          (Result n r k)    -> specifics (k ())
    
      triggers :: CGen ()
      triggers = do
          st <- gets widestWait
          mapM_ (\i -> indent ("trigger_t trig" ++ show i ++ ";")) [1..st]


genEnter :: SSM () -> CGen ()
genEnter ssm@(Procedure n _ _) = do
    let level = 19 + length n
    tell ["rar_" ++ n ++ "_t *enter_" ++ n ++ "( rar_t* caller"]
    indent level ", uint32_t priority"
    indent level ", uint8_t depth"
    censor closeParen $ mapM_ (\(t,name) -> indent level (", " ++ t ++ name)) $ getArgs ssm
    indent 0 "{"
    indent 4 $ "rar_" ++ n ++ "_t *rar = (rar_" ++ n ++ "_t *)"
    indent 8 $ "enter(sizeof(rar_" ++ n ++ "_t), step_" ++ n ++ ", caller, priority, depth);"
    mapM_ (\(_,name) -> indent 4 ("rar->" ++ name ++ " = " ++ name ++ ";")) $ getArgs ssm
    i <- gets widestWait
    mapM_ initTrigger [1..i]
    vars <- gets localVars
    mapM_ initLocal vars
    indent 0 "}"
  where
    getArgs :: SSM () -> [(String, String)]
    getArgs ssm = case ssm of
        (Procedure n _ k)             -> getArgs $ k ()
        (Argument n name (Left e) k)  -> (compileType (expType e) ++ " ", name) : getArgs (k ())
        (Argument n name (Right r) k) -> (compileType (snd r), name)            : getArgs (k ())
        _                             -> []
    
    initTrigger :: Int -> CGen ()
    initTrigger i = indent 4 $ "rar->trig" ++ show i ++ ".rar = (rar_t *) rar;"

    initLocal :: (String, Type) -> CGen ()
    initLocal (var,t) = if isReference t
        then indent 4 $ concat ["rar->", var, " = (", compileType t, ") malloc(sizeof("
                               , compileType (dereference t), "));"]
        else return ()

    indent :: Int -> String -> CGen ()
    indent level str = tell [replicate level ' ' ++ str]

    closeParen :: [String] -> [String]
    closeParen []     = []
    closeParen [x]    = [x ++ ")"]
    closeParen (x:xs) = x : closeParen xs

genStep :: SSM () -> CGen ()
genStep ssm@(Procedure n _ _) = do
    tell ["void step_" ++ n ++ "(rar_t *gen_rar)"]
    tell ["{"]
    indent 4 $ "rar_" ++ n ++ "_t *rar = (rar_" ++ n ++ "_t *) gen_rar;"
    indent 4   "switch(rar->pc) {"
    newCase 4
    instant 8 ssm
    indent 4   "; }"
    freeLocals
    indent 4 $ "leave((rar_t *) rar, sizeof(rar_" ++ n ++ "_t)); // Terminate"
    tell ["}"]
  where
      indent :: Int -> String -> CGen ()
      indent i str = tell [replicate i ' ' ++ str]

      newCase :: Int -> CGen ()
      newCase level = do
          numcase <- gets nextCase
          modify $ \st -> st {nextCase = nextCase st + 1}
          indent level $ "case " ++ show numcase ++ ":"

      freeLocals :: CGen ()
      freeLocals = do
          vars <- gets localVars
          forM_ vars $ \(v,t) ->
              if isReference t
                  then indent 4 $ "free(rar->" ++ v ++ ");"
                  else return ()
--          mapM_ (\s -> indent 4 ("free(rar->" ++ fst s ++ ");")) vars

      instant :: Int -> SSM () -> CGen ()
      instant level ssm = case ssm of
          (Return x)                -> return ()
          (NewRef (Just (_,n)) e k) -> do
              let t = simpleType (expType e)
              a <- compileLit e
              indent level $ concat ["initialize_", t, "(rar->", n, ", ", a, ");"]
              instant level $ k (n, Ref (expType e))
          (SetRef (r,_) e k)          -> do
              let t = simpleType (expType e)
              a <- compileLit e
              indent level $ concat ["assign_", t, "(rar->", r, ", rar->priority, ", a, ");"]
              instant level $ k ()
          (SetLocal (Var t e) v k)        -> do
              let t' = simpleType t
              a <- compileLit v
              indent level $ concat ["assign_", t', "(&rar->", e, ", rar->priority, ", a, ");"]
              instant level $ k ()
          (GetRef (r, t) (Just (_,n)) k) -> do
              let t' = simpleType (dereference t)
              indent level $ concat ["assign_", t', "(&rar->", n, ", rar->priority, rar->", r, "->value);"]
              instant level $ k (Var (dereference t) n)
          (GetRef r _ k) -> do
              error "not supporting immediate read from a reference yet"
          (If c thn (Just els) k) -> do
              a <- compileLit c
              indent level $ "if( " ++ a ++ " ) {"
              instant (level + 4) thn
              indent level "} else {"
              instant (level + 4) els
              indent level "}"
              instant level $ k ()
          (If c thn Nothing k)    -> do
              a <- compileLit c
              indent level $ "if( " ++ a ++ " ) {"
              instant (level + 4) thn
              indent level "}"
              instant level $ k ()
          (While c bdy k)         -> do
              a <- compileLit c
              indent level $ "while ( " ++ a ++ " ) {"
              instant (level + 4) bdy
              indent level "}"
              instant level $ k ()
          (After e (r,t) v k)         -> do
              let t' = simpleType (dereference t)
              e' <- compileLit e
              v' <- compileLit v
              indent level $ concat ["later_", t', "(rar->", r, ", now + ", e', ", ", v', ");"]
              instant level $ k ()
          (Changed (r,t) (Just (_,n)) k)           -> do
              --indent level $ "bool " ++ n ++ " = event_on((ct_t *) rar->" ++ r ++ ");"
              let t' = simpleType (dereference t)
              indent level $ concat ["assign_", t', "(&rar->", n, ", rar->priority, event_on((cv_t *) rar->", r, ");"]
              instant level $ k (Var TBool n)
          (Wait vars k)           -> do
              forM_ (zip vars [1..]) $ \((v,_),i) -> do
                  indent level $ "sensitize((cv_t*) rar->" ++ v ++ ", &rar->trig" ++ show i ++ ");"
              pc' <- gets nextCase
              indent level $ "rar->pc = " ++ show pc' ++ ";"
              indent level   "return;"
              newCase (level-4)
              forM_ (zip vars [1..]) $ \(v,i) -> do
                  indent level $ "desensitize(&rar->trig" ++ show i ++ ");"
              instant level $ k ()
          (Fork procs k)          -> do
              -- type error right now if you pass in local variables as arguments.
              -- they need to be of type cv_int, but are currently only int.
              if length procs > 1
                then do
                    let new_depth_corr = integerLogBase 2 (toInteger (length procs))
                    indent level $ "uint8_t  new_depth    = rar->depth - " ++ show new_depth_corr ++ ";"
                    indent level   "uint32_t pinc         = 1 << new_depth;"
                    indent level   "uint32_t new_priority = rar->priority;"
                    forks  <- mapM (compileFork level "fork") procs
                    let forks' = intersperse ["", "new_priority = += pinc;"] forks
                    mapM_ (mapM_ (indent level)) forks'
                    pc' <- gets nextCase
                    indent level $ "rar->pc = " ++ show pc' ++ ";"
                else do
                    pc' <- gets nextCase
                    indent level $ "rar->pc = " ++ show pc' ++ ";"
                    f <- compileFork level "call" (head procs)
                    mapM_ (indent level) f -- $ compileFork level "call" (head procs)
              indent level "return;"
              newCase (level-4)
              instant level $ k ()

          (Procedure _ _ k)       -> instant level $ k ()
          (Argument _ _ _ k)      -> instant level $ k ()
          (Result _ _ k)          -> instant level $ k ()

      compileFork :: Int -> String -> SSM () -> CGen [String]
      compileFork level method ssm@(Procedure n _ k) = do
          let constant_args = [ "(rar_t *) rar"
                              , "new_priority"
                              , "new_depth"
                              ]
          var_args      <- getArgs $ k ()
          let all_args      = constant_args ++ var_args
          let prefix        = method ++ "((rar_t *) enter_" ++ n ++ "( "
          let args          = map (\s -> replicate (length prefix - 1) ' ' ++ ", " ++ s) (tail all_args)
          return $ (prefix ++ head all_args) : args ++ ["));"]
        where
            getArgs :: SSM () -> CGen [String]
            getArgs (Argument _ x (Left e) k)  = do e' <- compileLit e
                                                    rest <- getArgs $ k ()
                                                    return $ e' : rest
            getArgs (Argument _ x (Right (r,_)) k) = do rest <- getArgs (k ())
                                                        return $ ("rar->" ++ r) : rest
            getArgs _                          = return []

      compileLit :: SSMExp -> CGen String
      compileLit e = case e of
          Var t s         -> if isReference t
                                then return $ "rar->" ++ s
                                else return $ "&rar->" ++ s
          Lit _ (LInt i)  -> return $ show i
          Lit _ (LBool b) -> if b then return "true" else return "false"
          UOp _ e Neg     -> do e' <- compileLit e
                                return $ "(-" ++ e' ++ ")"
          BOp _ e1 e2 op  -> do e1' <- compileLit e1
                                e2' <- compileLit e2
                                return $ "(" ++ e1' ++ ")" ++ compileOp op ++ "(" ++ e2' ++ ")"
        where
            compileOp :: BinOp -> String
            compileOp OPlus  = "+"
            compileOp OMinus = "-"
            compileOp OTimes = "*"
            compileOp OLT    = "<"
            compileOp OEQ    = "=="

      fresh :: CGen String
      fresh = do
          c <- gets nameGen
          modify $ \st -> st { nameGen = nameGen st + 1}
          return $ "v" ++ show c

compileType :: Type -> String
compileType TInt    = "cv_int_t"
compileType TBool   = "cv_bool_t"
compileType (Ref t) = compileType t ++ " *"

simpleType :: Type -> String
simpleType TInt  = "int"
simpleType TBool = "bool"
simpleType t     = error $ "not a simple type: " ++ show t

{- ********** testing some things out **********-}

{-
    Return  :: a -> SSM a

    -- | Variable/Stream operations
    NewRef   :: Name -> SSMExp -> (Reference -> SSM b) -> SSM b
    SetRef   :: Reference -> SSMExp -> (() -> SSM b) -> SSM b
    GetRef   :: Reference -> Name -> (SSMExp -> SSM b) -> SSM b
    SetLocal :: SSMExp -> SSMExp -> (() -> SSM b) -> SSM b
    
    -- | Control operations
    If     :: SSMExp -> SSM () -> Maybe (SSM ()) -> (() -> SSM b) -> SSM b
    While  :: SSMExp -> SSM () -> (() -> SSM b) -> SSM b
    
    -- | SSM specific operations
    After   :: SSMExp -> Reference -> SSMExp -> (() -> SSM b) -> SSM b
    Changed :: Reference -> Name -> (SSMExp -> SSM b) -> SSM b
    Wait    :: [Reference] -> (() -> SSM b) -> SSM b
    Fork    :: [SSM ()] -> (() -> SSM b) -> SSM b

    -- | Procedure construction
    Procedure :: Arg a => String -> (a -> b) -> (() -> SSM c) -> SSM c
    Argument  :: String -> String -> Either SSMExp Reference -> (() -> SSM b) -> SSM b
    Result    :: (Show a, Res a) => String -> a -> (() -> SSM b) -> SSM b
-}

data IR = Function String
    
        | If2 SSMExp String -- if !exp goto label
        | Label String
        | Goto String
        | Switch String
        | Case Int
        | Return2

        | Initialize (Either Reference SSMExp) SSMExp
        | Assign (Either Reference SSMExp) (Either Reference SSMExp)
        | Later Reference SSMExp SSMExp
        | EventOn Reference SSMExp

        | Sensitize Reference Int
        | Desensitize Int

        | Call String  [Either Reference SSMExp]
        | Fork2 [(String,  [Either Reference SSMExp])]
        | Enter String [Either Reference SSMExp]
        | Leave String

        | Malloc String Type
        | Free String

        | Blank
        | Literal String
  deriving Show

data TranspileState = TranspileState { nextLabel :: Int
                                     , ncase :: Int
                                     }
type TR a = StateT TranspileState (Writer [IR]) a

runTR :: SSM () -> [IR]
runTR ssm = let wr = evalStateT (transpile ssm) (TranspileState 0 0)
            in execWriter wr

compile :: SSM () -> String
compile ssm = let stmts = runTR ssm
                  withcases = (concat . wrapInSwitch . cases) stmts
                  --linear = concat $ intersperse [Blank] withcases
              in unlines $ execWriter (genFromIR withcases)

printIR :: [IR] -> String
printIR ir = intercalate "\n" $ map show ir

transpile :: SSM () -> TR ()
transpile ssm = case ssm of
    (Return x)                     -> do --liftIO $ putStrLn "return"
                                         return ()
    (NewRef (Just (_,n)) e k)      -> do --liftIO $ putStrLn "newref"
                                         let ref = (n, mkReference (expType e))
                                         tell [Initialize (Left ref) e]
                                         transpile $ k ref
    (SetRef r e k)                 -> do --liftIO $ putStrLn "setref"
                                         tell [Assign (Left r) (Right e)]
                                         transpile $ k ()
    (SetLocal e v k)               -> do --liftIO $ putStrLn "setlocal"
                                         tell [Assign (Right e) (Right v)]
                                         transpile $ k ()
    (GetRef (r, t) (Just (_,n)) k) -> do --liftIO $ putStrLn "getref"
                                         let var = Var (dereference t) n
                                         tell [Assign (Right var) (Left (r,t))]
                                         transpile $ k var
    (If c thn (Just els) k)        -> do --liftIO $ putStrLn "if"
                                         l  <- freshLabel
                                         l2 <- freshLabel
                                         tell [If2 c l]
                                         transpile thn
                                         tell [Goto l2]
                                         tell [Label l]
                                         transpile els
                                         --tell [Goto l2]
                                         tell [Label l2]
                                         transpile $ k ()
    (If c thn Nothing k)           -> do --liftIO $ putStrLn "return"
                                         l <- freshLabel
                                         tell [If2 c l]
                                         transpile thn
                                         --tell [Goto l]
                                         tell [Label l]
                                         transpile $ k ()
    (While c bdy k)                -> undefined -- TODO
    (After e r v k)                -> do --liftIO $ putStrLn "after"
                                         tell [Later r e v]
                                         transpile $ k ()
    (Changed r (Just (_,n)) k)     -> do --liftIO $ putStrLn "changed"
                                         let var = Var (dereference (snd r)) n
                                         tell [EventOn r var]
                                         transpile $ k var
    (Wait vars k)                  -> do --liftIO $ putStrLn "wait"
                                         mapM_ (\(v,i) -> tell [Sensitize v i]) (zip vars [1..])
                                         mapM_ (\(_,i) -> tell [Desensitize i]) (zip vars [1..])
                                         transpile $ k ()
    (Fork procs k)                 -> do --liftIO $ putStrLn "fork"
                                         forks <- mapM compileFork procs
                                         if length forks == 1
                                             then tell $ map (uncurry Call) forks
                                             else tell [Fork2 forks]
                                         transpile $ k ()
    (Procedure n _ k)              -> do --liftIO $ putStrLn "function"
                                         let (_, rest) = getParams $ k ()
                                         tell [Function n]
                                         transpile rest
    (Result n r k)                 -> {-liftIO (putStrLn "result") >>-} tell [Leave n]
  where
      getParams :: SSM () -> ([Either Reference SSMExp], SSM ())
      getParams (Argument _ name (Left e) k) =
          let arg          = Right $ Var (expType e) name
              (args, rest) = getParams $ k ()
          in (arg:args, rest)
      getParams (Argument _ name (Right (r,t)) k) =
          let arg          = Left $ (name,t)
              (args, rest) = getParams $ k ()
          in (arg:args, rest)
      getParams s = ([], s)

      compileFork :: SSM () -> TR (String, [Either Reference SSMExp])
      compileFork ssm = let fun  = getFun ssm
                            args = getArgs ssm
                        in return $ (fun, args)
        where
            getFun :: SSM () -> String
            getFun (Procedure n _ _) = n
            getFun _                 = error $ "not a function"
      
            getArgs :: SSM () -> [Either Reference SSMExp]
            getArgs (Procedure _ _ k)             = getArgs $ k ()
            getArgs (Argument _ name (Left e) k)  = Right e : getArgs (k ())
            getArgs (Argument _ name (Right r) k) = Left  r : getArgs (k ())
            getArgs s                             = []
    
      freshLabel :: TR String
      freshLabel = do
          st <- get
          put $ st { nextLabel = nextLabel st + 1 }
          return $ "L" ++ show (nextLabel st)

      emitNextCase :: TR ()
      emitNextCase = do
          st <- get
          put $ st { ncase = ncase st + 1 }
          tell [Case (ncase st)]
    
      emitPC :: Int -> TR ()
      emitPC i = tell [Literal ("rar->pc = " ++ show i)]

      emitReturn :: TR ()
      emitReturn = tell [Return2]

cases :: [IR] -> [[IR]]
cases [] = []
cases xs = case myTakeWhile pred xs of
    ([], []) -> []
    (x, xs') -> x : cases xs'
  where
      pred :: IR -> Bool
      pred (Call _ _)      = True
      pred (Fork2 _)       = True
      pred (Sensitize _ _) = True
      pred _               = False

      myTakeWhile :: (a -> Bool) -> [a] -> ([a], [a])
      myTakeWhile p [] = ([], [])
      myTakeWhile p (x:xs) = if p x
          then ([x], xs)
          else let (x', xs') = myTakeWhile p xs
               in (x:x', xs')

wrapInSwitch :: [[IR]] -> [[IR]]
wrapInSwitch xs = rewriteHead y : ys
  where
      wcase i = Case i
      incPC i = Literal $ "rar->pc = " ++ show (i + 1)
      (y:ys) = map (\(c,i) -> concat [[wcase i], c, [incPC i], [Return2]]) (zip xs [0..])

      rewriteHead :: [IR] -> [IR]
      rewriteHead xs = concat [[xs !! 1], [Switch "rar->pc"], [Case 0], drop 2 xs]

printCases :: [[IR]] -> IO ()
printCases xs = putStrLn $ intercalate "\n\n" strings
    where
        strings = map printIR xs

-- TODO now I am manually inserting empty spaces after goto, label and return. Is this the best way to do it?
genFromIR :: [IR] -> Writer [String] ()
genFromIR [] = return ()
genFromIR (x:xs) = case x of
    Function name -> do indent 0 $ concat ["void step_", name, "(rar_t *gen_rar) {"]
                        indent 4 $ concat ["rar_", name, "_t *rar = (rar_", name, "_t *) gen_rar;"]
                        genFromIR xs
    
    If2 notexp label -> do indent 12 $ "if (!(" ++ compLit notexp ++ ")) goto " ++ label ++ ";"
                           genFromIR xs
    Label label      -> do indent 8 $ label ++ ":"
                           genFromIR xs
    Goto label       -> do indent 12 $ "goto " ++ label ++ ";"
                           indent 0 ""
                           genFromIR xs
    Switch scrut     -> do indent 4 $ "switch(" ++ scrut ++ ") {"
                           genFromIR xs
    Case num         -> do indent 8 $ "case " ++ show num ++ ":"
                           genFromIR xs
    Return2          -> do indent 12 $ "return;"
                           indent 0 ""
                           genFromIR xs

    Initialize var val  -> let st = simpleType var
                               v  = compVar var
                           in do indent 12 $ concat ["initialise_", st, "(", v, ", ", compLit val, ");"]
                                 genFromIR xs
    Assign var val      -> let st   = simpleType var
                               var' = compVar var
                               val' = compVal val
                           in do indent 12 $ concat ["assign_", st, "(", var', ", rar->priority, ", val', ");"]
                                 genFromIR xs
    Later ref delay val -> let st     = simpleType (Left ref)
                               var    = compVar (Left ref)
                               time   = compLit delay
                               newval = compLit val
                           in do indent 12 $ concat ["later_", st, "(", var, ", ", time, ", ", newval, ");"]
                                 genFromIR xs
    EventOn ref var     -> let st = simpleType (Right var)
                               ref' = compVar (Left ref)
                               var' = compVar (Right var)
                               eventon = concat ["event_on((cv_t *)", ref', ")"]
                           in do indent 12 $ concat ["assign_", st, "(", var', ", rar->priority, ", eventon, ");"]
                                 genFromIR xs

    Sensitize ref trig -> let var = compVar (Left ref)
                          in do indent 12 $ concat ["sensitize((cv_t *) ", var, ", &rar->trig" ++ show trig ++ ");"]
                                genFromIR xs
    Desensitize trig   -> do indent 12 $ concat ["desensitize(&rar->trig" ++ show trig ++ ");"]
                             genFromIR xs

    Call fun args    -> let args' = map compArg args
                        in do indent 12 $ concat (["call((rar_t *) enter_", fun, "((rar_t *) rar, rar->priority, rar->depth, "] ++ intersperse ", " args' ++ ["));"])
                              genFromIR xs
    Fork2 procs      -> genFromIR xs
    Enter fun params -> genFromIR xs
    Leave fun        -> do indent 0 ""
                           indent 8 $ concat ["leave((rar_t *) rar, sizeof(rar_", fun, "_t));"]
                           indent 4 "}"
                           indent 0 "}"
                           --genFromIR xs

    Malloc var typ -> genFromIR xs
    Free var       -> genFromIR xs

    Blank -> indent 0 ""
    Literal lit -> indent 12 (lit ++ ";") >> genFromIR xs
  where
      indent :: Int -> String -> Writer [String] ()
      indent i str = tell [replicate i ' ' ++ str]

      compLit :: SSMExp -> String
      compLit e = case e of
          Var _ e        -> "rar->" ++ e ++ ".value"
          Lit _ l        -> case l of
                              LInt i      -> show i
                              LBool True  -> "true"
                              LBool False -> "false"
          UOp _ e op     -> case op of
                              Neg -> "(-" ++ compLit e ++ ")"
          BOp _ e1 e2 op -> case op of
                              OPlus  -> "(" ++ compLit e1 ++ ") + (" ++ compLit e2 ++ ")"
                              OMinus -> "(" ++ compLit e1 ++ ") - (" ++ compLit e2 ++ ")"
                              OTimes -> "(" ++ compLit e1 ++ ") * (" ++ compLit e2 ++ ")"
                              OLT    -> "(" ++ compLit e1 ++ ") < (" ++ compLit e2 ++ ")"
                              OEQ    -> "(" ++ compLit e1 ++ ") == (" ++ compLit e2 ++ ")"

      compVar :: Either Reference SSMExp -> String
      compVar (Left (r,_))      = "rar->"  ++ r
      compVar (Right (Var _ n)) = "&rar->" ++ n
      compVar (Right e)         = error $ "not a variable: " ++ show e

      compVal :: Either Reference SSMExp -> String
      compVal (Left (r,_))      = "rar->" ++ r ++ "->value"
      compVal (Right (Var _ v)) = "rar->" ++ v ++ ".value"
      compVal (Right e)         = compLit e

      compArg :: Either Reference SSMExp -> String
      compArg (Left (r,_))      = "rar->" ++ r
      compArg (Right (Var _ v)) = "rar->" ++ v ++ ".value"
      compArg (Right e)         = compVal (Right e)

      simpleType :: Either Reference SSMExp -> String
      simpleType (Left (r,t)) = prtyp $ dereference t
      simpleType (Right e)    = prtyp $ expType e

      prtyp :: Type -> String
      prtyp TInt  = "int"
      prtyp TBool = "bool"
      prtyp t     = error $ "not a simple type: " ++ show t