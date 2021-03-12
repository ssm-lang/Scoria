module CodeGen where

import Control.Monad.State
import Control.Monad.Writer
import GHC.Float
import Data.List

import Core

data IR = Function String [String]

        | TypedefStruct
        | FieldDec Type String
        | CloseTypeDef String
    
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

        | Sensitize [(Reference, Int)]
        | Desensitize Int

        | Call String  [Either Reference SSMExp]
        | Fork2 [(String,  [Either Reference SSMExp])]
        | Leave String

        | Malloc String Type
        | Free Reference

        | Blank
        | Literal Int String
  deriving Show

data TRState = TRState { nextLabel :: Int
                       , ncase :: Int
                       , numwaits :: Int
                       , localrefs :: [Reference]
                       , generated :: [String]
                       , toGenerate :: [SSM ()]
                       }
type TR a = StateT TRState (Writer [IR]) a

compile :: SSM () -> String
compile ssm = let wr     = evalStateT generateProcedures (TRState 0 0 0 [] [] [ssm])
                  (a, _) = runWriter wr
              in unlines a

generateProcedures :: TR [String]
generateProcedures = do
    st <- gets toGenerate
    if null st
        then return []
        else do
            resetState
            gen <- gets generated
            let (toc, rest) = (head st, tail st)
            if name toc `elem` gen
                then do modify $ \st -> st { toGenerate = rest}
                        generateProcedures
                else do modify $ \state -> state { toGenerate = tail (toGenerate state)}
                        tri  <- ssmToC toc
                        modify $ \state -> state { generated  = name toc : generated state}
                        rest <- generateProcedures
                        return $ rest ++ tri
  where
      name :: SSM () -> String
      name (Procedure n _ _) = n

      resetState :: TR ()
      resetState = modify $ \st -> st { nextLabel = 0, ncase = 0, numwaits = 0, localrefs = []}

ssmToC :: SSM () -> TR [String]
ssmToC ssm = do
    struct <- censor (const []) $ snd <$> listen (genIRStruct ssm)
    enter  <- censor (const []) $ snd <$> listen (genIREnter ssm)
    step   <- censor (const []) $ snd <$> listen (genIRStep ssm)

    let struct' = execWriter (genCFromIR struct)
    let enter'  = execWriter (genCFromIR enter)
    let step'   = execWriter (genCFromIR ((concat . wrapInSwitch . cases) step))

    return [unlines struct', unlines enter', unlines step']
  where
      cases :: [IR] -> [[IR]]
      cases [] = []
      cases xs = case myTakeWhile pred xs of
          ([], []) -> []
          (x, xs') -> x : cases xs'
        where
            pred :: IR -> Bool
            pred (Call _ _)      = True
            pred (Fork2 _)       = True
            pred (Sensitize _ )  = True
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
            incPC i = Literal 12 $ "rar->pc = " ++ show (i + 1)
            (y:ys) = map (\(c,i) -> concat [[wcase i], c, [incPC i, Return2, Blank]]) (zip xs [0..])

            rewriteHead :: [IR] -> [IR]
            rewriteHead xs = concat [[xs !! 1], [xs !! 2], [Switch "rar->pc"], [Case 0], drop 3 xs]

genIRStruct :: SSM () -> TR ()
genIRStruct ssm@(Procedure n _ _) = do
    tell [ TypedefStruct
         , Literal 4 "/* Procedure generic fields */"
         , Literal 4 "void (*step)(rar_t *)"
         , Literal 4 "uint16_t  pc"
         , Literal 4 "rar_t    *caller"
         , Literal 4 "uint16_t  children"
         , Literal 4 "uint32_t  priority"
         , Literal 4 "uint8_t   depth"
         , Literal 4 "bool      scheduled"
         , Literal 4 "/* Procedure specific fields */"
         ]
    go ssm
    triggers
    tell [ CloseTypeDef n]
  where
      go :: SSM () -> TR ()
      go ssm = case ssm of
        (Return x)                     -> return ()
        (NewRef (Just (_,n)) e k)      -> do let ref = (n, mkReference (expType e))
                                             tell [FieldDec (mkReference (expType e)) n]
                                             go $ k ref
        (SetRef r e k)                 -> go $ k ()
        (SetLocal e v k)               -> go $ k ()
        (GetRef (r, t) (Just (_,n)) k) -> do let var = Var (dereference t) n
                                             tell [FieldDec (dereference t) n]
                                             go $ k var
        (If c thn (Just els) k)        -> go $ thn >> els >> k ()
        (If c thn Nothing k)           -> go $ thn >> k ()
        (While c bdy k)                -> go $ bdy >> k ()
        (After e r v k)                -> go $ k ()
        (Changed r (Just (_,n)) k)     -> do let var = Var TBool n
                                             tell [FieldDec TBool n]
                                             go $ k var
        (Wait vars k)                  -> do modify $ \st -> st { numwaits = max (numwaits st) (length vars)}
                                             go $ k ()
        (Fork procs k)                 -> go $ k ()
        (Procedure n _ k)              -> go $ k ()
        (Argument _ n (Left e) k)      -> do tell [FieldDec (expType e) n]
                                             go $ k ()
        (Argument _ n (Right (r,t)) k) -> do tell [FieldDec t n]
                                             go $ k ()
        (Result n r k)                 -> go $ k ()

      triggers :: TR ()
      triggers = do
          num <- gets numwaits
          mapM_ (\i -> tell [Literal 4 ("trigger_t trig" ++ show i)]) [1..num]

genIREnter :: SSM () -> TR ()
genIREnter ssm = case ssm of
    (Return x)                     -> return ()
    (NewRef (Just (_,n)) e k)      -> do modify $ \st -> st { localrefs = localrefs st ++ [(n, mkReference (expType e))]}
                                         tell [Malloc n (expType e)]
                                         genIREnter $ k (n, mkReference (expType e))
    (SetRef r e k)                 -> genIREnter $ k ()
    (SetLocal e v k)               -> genIREnter $ k ()
    (GetRef (r, t) (Just (_,n)) k) -> genIREnter $ k (Var (dereference t) n)
    (If c thn (Just els) k)        -> genIREnter $ thn >> els >> k ()
    (If c thn Nothing k)           -> genIREnter $ thn >> k ()
    (While c bdy k)                -> genIREnter $ bdy >> k ()
    (After e r v k)                -> genIREnter $ k ()
    (Changed r (Just (_,n)) k)     -> genIREnter $ k (Var TBool n)
    (Wait vars k)                  -> do modify $ \st -> st { numwaits = max (numwaits st) (length vars)}
                                         genIREnter $ k ()
    (Fork procs k)                 -> genIREnter $ k ()
    (Procedure n _ k)              -> do 
        let sig = "rar_" ++ n ++ "_t *enter_" ++ n
        let args = [ "rar_t *caller"
                   , "uint32_t priority"
                   , "uint8_t depth"
                   ] ++ getParams (k ())
        tell [Function sig args]
        let genenter = concat ["rar_"
                              , n
                              , "_t *rar = (rar_"
                              , n
                              , "_t *) enter(sizeof(rar_"
                              , n
                              , "_t), step_"
                              , n
                              , ", caller, priority, depth)"]
        tell [Literal 4 genenter]
        assignParams $ k ()
        initTriggers
        genIREnter   $ k ()
    (Argument _ _ _ k)             -> genIREnter $ k ()
    (Result n r k)                 -> do tell [Literal 0 "}"]
                                         genIREnter $ k ()
  where
      getParams :: SSM () -> [String]
      getParams (Argument _ name (Left e) k)      = (prtyp (expType e) ++ " " ++ name)       : getParams (k ())  
      getParams (Argument _ name (Right (r,t)) k) = (cvtype (dereference t) ++ " *" ++ name) : getParams (k ())
      getParams s                                 = []

      assignParams :: SSM () -> TR ()
      assignParams (Argument _ name (Left e) k)      = do
          tell [Literal 4 (concat ["initialise_", prtyp (expType e), "(&rar->", name, ", ", name, ")"])]
          assignParams $ k ()  
      assignParams (Argument _ name (Right (r,t)) k) = do
          tell [Literal 4 ("rar->" ++ name ++ " = " ++ name)]
          assignParams $ k ()
      assignParams s = return ()

      initTriggers :: TR ()
      initTriggers = do
          nw <- gets numwaits
          mapM_ (\i -> tell [Literal 4 ("rar->trig" ++ show i ++ ".rar = (rar_t *) rar")]) [1..nw]

      cvtype :: Type -> String
      cvtype t = "cv_" ++ prtyp t ++ "_t"

      prtyp :: Type -> String
      prtyp TInt = "int"
      prtyp TBool = "bool"
      prtyp t     = error $ "not a simple type: " ++ show t


genIRStep :: SSM () -> TR ()
genIRStep ssm = case ssm of
    (Return x)                     -> do return ()
    (NewRef (Just (_,n)) e k)      -> do let ref = (n, mkReference (expType e))
                                         tell [Initialize (Left ref) e]
                                         genIRStep $ k ref
    (SetRef r e k)                 -> do tell [Assign (Left r) (Right e)]
                                         genIRStep $ k ()
    (SetLocal e v k)               -> do tell [Assign (Right e) (Right v)]
                                         genIRStep $ k ()
    (GetRef (r, t) (Just (_,n)) k) -> do let var = Var (dereference t) n
                                         tell [Assign (Right var) (Left (r,t))]
                                         genIRStep $ k var
    (If c thn (Just els) k)        -> do l  <- freshLabel
                                         l2 <- freshLabel
                                         tell [If2 c l]
                                         genIRStep thn
                                         tell [Goto l2, Blank]
                                         tell [Label l]
                                         genIRStep els
                                         tell [Blank, Label l2]
                                         genIRStep $ k ()
    (If c thn Nothing k)           -> do l <- freshLabel
                                         tell [If2 c l]
                                         genIRStep thn
                                         tell [Blank, Label l]
                                         genIRStep $ k ()
    (While c bdy k)                -> undefined -- TODO
    (After e r v k)                -> do tell [Later r e v]
                                         genIRStep $ k ()
    (Changed r (Just (_,n)) k)     -> do let var = Var (dereference (snd r)) n
                                         tell [EventOn r var]
                                         genIRStep $ k var
    (Wait vars k)                  -> do tell [Sensitize (zip vars [1..])]
                                         mapM_ (\(_,i) -> tell [Desensitize i]) (zip vars [1..])
                                         genIRStep $ k ()
    (Fork procs k)                 -> do forks <- mapM compileFork procs
                                         if length forks == 1
                                             then tell $ map (uncurry Call) forks
                                             else tell [Fork2 forks]
                                         forkProcs procs
                                         genIRStep $ k ()
    (Procedure n _ k)              -> do let (_, rest) = getParams $ k ()
                                         tell [Function ("void step_" ++ n) ["rar_t *gen_rar"]]
                                         tell [Literal 4 $ concat ["rar_", n, "_t *rar = (rar_", n, "_t *) gen_rar"]]
                                         genIRStep rest
    (Result n r k)                 -> do tell [Blank]
                                         lrefs <- gets localrefs
                                         mapM_ (\ref -> tell [Free ref]) lrefs
                                         tell [Leave n]
  where
      getParams :: SSM () -> ([Either Reference SSMExp], SSM ())
      getParams (Argument _ name (Left e) k) =
          let arg          = Right $ Var (expType e) name
              (args, rest) = getParams $ k ()
          in (arg:args, rest)
      getParams (Argument _ name (Right (r,t)) k) =
          let arg          = Left (name,t)
              (args, rest) = getParams $ k ()
          in (arg:args, rest)
      getParams s = ([], s)

      compileFork :: SSM () -> TR (String, [Either Reference SSMExp])
      compileFork ssm = let fun  = getFun ssm
                            args = getArgs ssm
                        in return (fun, args)
        where
            getFun :: SSM () -> String
            getFun (Procedure n _ _) = n
            getFun _                 = error "not a function"
      
            getArgs :: SSM () -> [Either Reference SSMExp]
            getArgs (Procedure _ _ k)             = getArgs $ k ()
            getArgs (Argument _ name (Left e) k)  = Right e : getArgs (k ())
            getArgs (Argument _ name (Right r) k) = Left  r : getArgs (k ())
            getArgs s                             = []
    
      forkProcs :: [SSM ()] -> TR ()
      forkProcs xs = do
          forM_ xs $ \ssm@(Procedure n _ _) -> do
              gen <- gets generated
              if n `elem` gen
                  then return ()
                  else modify $ \st -> st { toGenerate = ssm : toGenerate st}

      freshLabel :: TR String
      freshLabel = do
          st <- get
          put $ st { nextLabel = nextLabel st + 1 }
          return $ "L" ++ show (nextLabel st)

genCFromIR :: [IR] -> Writer [String] ()
genCFromIR [] = return ()
genCFromIR (Blank:Blank:xs) = genCFromIR (Blank:xs)
genCFromIR (x:xs) = case x of
    Function name args -> do indent 0 $ name ++ "(" ++ intercalate ", " args ++ ") {"
                             genCFromIR xs

    TypedefStruct         -> do indent 0 "typedef struct {"
                                genCFromIR xs
    FieldDec ftyp fname   -> do let ct = cvtyp ftyp
                                indent 4 $ concat [ct, fname, ";"]
                                genCFromIR xs
    CloseTypeDef tdefname -> do indent 0 $ "} rar_" ++ tdefname ++ "_t;"
                                genCFromIR xs
    
    If2 notexp label -> do indent 12 $ "if (!(" ++ compLit notexp ++ ")) goto " ++ label ++ ";"
                           genCFromIR xs
    Label label      -> do indent 8 $ label ++ ":"
                           genCFromIR xs
    Goto label       -> do indent 12 $ "goto " ++ label ++ ";"
                           genCFromIR xs
    Switch scrut     -> do indent 4 $ "switch(" ++ scrut ++ ") {"
                           genCFromIR xs
    Case num         -> do indent 8 $ "case " ++ show num ++ ":"
                           genCFromIR xs
    Return2          -> do indent 12 "return;"
                           genCFromIR xs

    Initialize var val  -> let st = simpleType var
                               v  = compVar var
                           in do indent 12 $ concat ["initialise_", st, "(", v, ", ", compLit val, ");"]
                                 genCFromIR xs
    Assign var val      -> let st   = simpleType var
                               var' = compVar var
                               val' = compVal val
                           in do indent 12 $ concat ["assign_", st, "(", var', ", rar->priority, ", val', ");"]
                                 genCFromIR xs
    Later ref delay val -> let st     = simpleType (Left ref)
                               var    = compVar (Left ref)
                               time   = compLit delay
                               newval = compLit val
                           in do indent 12 $ concat ["later_", st, "(", var, ", now + ", time, ", ", newval, ");"]
                                 genCFromIR xs
    EventOn ref var     -> let st = simpleType (Right var)
                               ref' = compVar (Left ref)
                               var' = compVar (Right var)
                               eventon = concat ["event_on((cv_t *)", ref', ")"]
                           in do indent 12 $ concat ["assign_", st, "(", var', ", rar->priority, ", eventon, ");"]
                                 genCFromIR xs

    Sensitize vars -> do
        forM_ vars $ \(ref,trig) -> do
            let var = compVar (Left ref)
            indent 12 $ concat ["sensitize((cv_t *) ", var, ", &rar->trig", show trig, ");"]
        genCFromIR xs
    Desensitize trig   -> do indent 12 $ concat ["desensitize(&rar->trig", show trig, ");"]
                             genCFromIR xs

    Call fun args    -> let args' = map compArg args
                        in do indent 12 $ concat (["call((rar_t *) enter_", fun, "((rar_t *) rar, rar->priority, rar->depth, "] ++ intersperse ", " args' ++ ["));"])
                              genCFromIR xs
    Fork2 procs      -> do let new_depth = integerLogBase 2 (toInteger (length procs))
                           indent 12 $ "uint8_t new_depth = rar->depth - " ++ show new_depth ++ ";"
                           indent 12   "uint32_t pinc = 1 << new_depth;"
                           indent 12   "uint32_t new_priority = rar->priority;"
                           let forks = intercalate ["new_priority += pinc;"] $ map compileFork procs
                           mapM_ (indent 12) forks
                           genCFromIR xs
    Leave fun        -> do indent 8 $ concat ["leave((rar_t *) rar, sizeof(rar_", fun, "_t));"]
                           indent 4 "}"
                           indent 0 "}"

    Malloc var typ -> do let st    = prtyp typ
                         let ctp = cvtyp (mkReference typ)
                         let ct = cvtyp typ --"cv_" ++ st ++ "_t"
                         indent 4 $ concat ["rar->", var, " = (", ctp, ") malloc(sizeof(", init ct, "));"]
                         genCFromIR xs
    Free (var, _)  -> do indent 8 $ concat ["free(", var, ");"]
                         genCFromIR xs

    Blank -> indent 0 "" >> genCFromIR xs
    Literal i lit -> indent i (lit ++ ";") >> genCFromIR xs
  where
      indent :: Int -> String -> Writer [String] ()
      indent i str = tell [replicate i ' ' ++ str]

      compileFork :: (String, [Either Reference SSMExp]) -> [String]
      compileFork (fun, ar) = let f  = "fork((rar_t *) enter_" ++ fun ++ "( "
                                  lf = length f
                                  ars = intercalate ('\n' : replicate (length f + 10) ' ' ++ ", ") args
                                in [f ++ ars ++ "));"]
        where
            args :: [String]
            args = ["(rar_t *) rar", "new_priority", "new_depth"] ++ map compArg ar

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

      cvtyp :: Type -> String
      cvtyp t = if isReference t
          then concat ["cv_", prtyp (dereference t), "_t *"]
          else concat ["cv_", prtyp t, "_t "]