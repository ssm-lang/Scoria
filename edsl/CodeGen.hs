module CodeGen (compile) where

import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy
import GHC.Float
import Data.List
import Data.Maybe
import Data.Either

import Core

-- SSM () -> [IR]
-- [IR] -> [IR]
-- | One value of this datatype corresponds to one statement/line in the C code
data IR = Function String [String]

        | FieldDec Type String (Maybe String) -- Last string is a comment

        | IfFalse SSMExp String -- if !exp goto label
        | Label String      -- label
        | Goto String       -- unconditional jump
        | Case Int          -- case -the int-:
        | Ret

        -- Methods from the SSM runtime
        | Initialize (Either Reference SSMExp)                       -- Initialize a variable
        | Assign (Either Reference SSMExp) (Either Reference SSMExp)  -- Assign to variables in the struct
        | Later Reference SSMExp SSMExp                               -- Delayed assignment
        | EventOn Reference SSMExp                                    -- Has the reference been written
        | Sensitize [(Reference, Int)] -- [(reference, triggernumber)]
        | Desensitize Int              -- Stop waiting for a variable
        | Call String  [Either Reference SSMExp]       -- (Procedure, arguments)
        | ForkProcedures [(String,  [Either Reference SSMExp])] -- [(Procedure, arguments)]
        | Leave String                                 -- Name of the procedure

        | Blank              -- Blank linkes for readability
        | Literal Int String -- Verbatim code
        | LiteralNoSemi Int String
  deriving Show

data TRState = TRState { nextLabel  :: Int         -- ^ Counter to generate fresh labels
                       , nextVar    :: Int         -- ^ Counter to generate fresh names
                       , ncase      :: Int         -- ^ Which number has the next case?
                       , numwaits   :: Int         -- ^ The size of the widest wait
                       , localrefs  :: [Reference] -- ^ Local references declared with var
                       , code       :: ([IR], [IR], [IR])  -- ^ Struct, enter, step
                       , generated  :: [String]    -- ^ Which procedures are already generated?
                       , toGenerate :: [SSM ()]    -- ^ Which remain to be generated?
                       }

type TR a = State TRState a

-- | Main compiler entrypoint. Will output a C-file as a single string.
compile :: Bool -> Maybe Int -> SSM () -> String
compile b d ssm = let ( structs
                       , prototypes
                       , entersteps
                       ) = evalState generateProcedures (TRState 0 0 0 0 [] ([], [], []) [] [ssm])
                      testmain = if b then execWriter (generateMain (runSSM ssm) d) else [""]
                  in unlines $ [ "#include \"peng-platform.h\""
                               , "#include \"peng.h\""
                               , "#include <stdio.h>"
                               , ""
                               ] ++ structs ++ prototypes ++ entersteps ++ testmain

-- | Return a list of all triples (struct, enter, step)
generateProcedures :: TR ([String], [String], [String])
generateProcedures = do
    st <- gets toGenerate
    -- If there are no procedures left to generate C code for the function terminates
    if null st
        then return ([],[],[])
        else do
            -- Otherwise the procedure specific state is reset and the next procedure is fetched
            resetState
            gen <- gets generated
            let (toc, rest) = (head st, tail st)
            let p = head $ runSSM toc
            {- If we have not already generated code for this procedure we will do so, otherwise
            we will forget about the procedure and recursively call generateProcedures. -}
            if name p `elem` gen
                then do modify $ \st -> st { toGenerate = rest}
                        generateProcedures
                else do -- Remove the procedure to generate code for from the toGenerate list
                        modify $ \state -> state { toGenerate = tail (toGenerate state)}
                        (struct, prototype, enterstep)  <- ssmToC toc
                        {- After code has been generated for the procedure we add its name
                        to the list of generated procedures. -}
                        modify $ \state -> state { generated  = name p : generated state}
                        (structs, prototypes, entersteps) <- generateProcedures
                        return $ (struct ++ "":structs, prototype ++ "":prototypes, enterstep ++ "":entersteps)
                        --return $ rest ++ tri
  where
      -- | Get the name of a procedure
      name :: SSMStm -> String
      name (Procedure n) = n

      -- | Reset the procedure specific parts of the state before generating C for the next procedure
      resetState :: TR ()
      resetState = modify $ \st -> st { nextLabel = 0
                                      , nextVar   = 0
                                      , ncase     = 0
                                      , numwaits  = 0
                                      , localrefs = []
                                      , code      = ([], [], [])}

      -- | Take a single SSM program and generate a struct, function prototypes and the enter+step function.
      ssmToC :: SSM () -> TR ([String], [String], [String])
      ssmToC ssm = do
          genIR ssm
          (struct, enter, step) <- gets code
          lrefs                 <- gets localrefs

          {- Generate C code from the IR instructions that were emitted. The step function needs to
          be converted to a version that returns at blocking points before C can be generated. -}
          let (struct', _)  = execWriter (genCFromIR struct lrefs)
          let (enter',  p2) = execWriter (genCFromIR enter lrefs)
          let (step',   p3) = execWriter (genCFromIR step lrefs)

          return (struct', [p2,p3], concat [enter', [""], step'])

--          return [unlines struct', p2, p3, "", unlines enter', unlines step']

genIR :: SSM () -> TR ()
genIR ssm = stmts $ runSSM ssm
  where
      appendStruct :: IR -> TR ()
      appendStruct ir = modify $ \st -> st {code = code st <> ([ir], [], [])}

      prependStruct :: [IR] -> TR ()
      prependStruct ir = modify $ \st -> st {code = (ir, [], []) <> code st}

      appendEnter :: IR -> TR ()
      appendEnter ir = modify $ \st -> st {code = code st <> ([], [ir], [])}

      appendStep :: IR -> TR ()
      appendStep ir = modify $ \st -> st {code = code st <> ([], [], [ir])}

      stmts :: [SSMStm] -> TR ()
      stmts []       = return ()
      stmts (ssm:xs) = case ssm of
          NewRef name e -> do
              (n,c) <- case name of
                  Captured (f,x,y) n -> let comment = concat [" // Declared at ("
                                                            , show x
                                                            , ", ", show y
                                                            , ") in file ", file f
                                                            ]
                                        in return (n, comment)
                  Fresh n            -> let comment = " // generated name"
                                        in return (n, comment)
              let reftype = mkReference $ expType e
              let ref     = (n, reftype)
              modify $ \st -> st { localrefs = localrefs st ++ [ref]}

              {- struct -}
              appendStruct $ FieldDec (expType e) n (Just c)

              {- step -}
              appendStep $ Initialize (Left ref)
              appendStep $ Assign (Left ref) (Right e)
              stmts xs

          SetRef r e -> do appendStep $ Assign (Left r) (Right e)
                           stmts xs
          
          SetLocal e v -> do appendStep $ Assign (Right e) (Right v)
                             stmts xs

          GetRef (r, t) name -> do
              (n,c) <- case name of
                  Captured (f,x,y) n -> let comment = concat [" // Declared at ("
                                                           , show x, ", ", show y
                                                           , ") in file ", file f
                                                           ]
                                        in return (n, comment)
                  Fresh n            -> let comment = " // generated name"
                                        in return (n, comment)
              let vartyp = dereference t
              let var = Var vartyp n

              {- struct -}
              appendStruct $ FieldDec vartyp n (Just c)

              {- step -}
              appendStep $ Assign (Right var) (Left (r,t))      

              stmts xs

          If c thn (Just els) -> do

              {- step -}
              l1 <- freshLabel
              l2 <- freshLabel

              appendStep $ IfFalse c l1
              stmts $ runSSM thn
              appendStep $ Goto l2
              appendStep   Blank
              appendStep $ Label l1
              stmts $ runSSM els
              appendStep   Blank
              appendStep $ Label l2

              stmts xs
          
          If c thn Nothing -> do

              {- step -}
              l <- freshLabel
              
              appendStep $ IfFalse c l
              stmts $ runSSM thn
              appendStep   Blank
              appendStep $ Label l

              stmts xs

          While c bdy -> do

              {- step -}
              l1 <- freshLabel
              l2 <- freshLabel

              appendStep $ Label l1
              appendStep $ IfFalse c l2
              stmts $ runSSM bdy
              appendStep $ Goto l1
              appendStep $ Label l2
              
              stmts xs

          After e r v -> do appendStep $ Later r e v
                            stmts xs

          Changed r name -> do
              (n,c) <- case name of
                  Captured (f,x,y) n -> let comment = concat [" // Declared at ("
                                                           , show x, ", "
                                                           , show y, " in file ", file f
                                                           ]
                                        in return (n, comment)
                  Fresh n            -> let comment = " // generated name"
                                        in return (n, comment)
              let var = Var TBool n

              {- struct -}
              appendStruct $ FieldDec TBool n (Just c)

              {- step -}
              appendStep $ EventOn r var
              stmts xs
        
          Wait vars -> do
              modify $ \st -> st { numwaits = max (numwaits st) (length vars)}

              {- step -}
              appendStep $ Sensitize $ zip vars [1..]
              incPC
              appendStep $ Ret
              nextCase
              forM_ [1..length vars] $ \i ->
                  appendStep (Desensitize i)
              stmts xs

          Fork procs -> do

              {- step -}
              let forks = map (compileFork . runSSM) procs

              if length procs == 1
                  then do incPC
                          appendStep $ uncurry Call (head forks)
                  else do appendStep $ ForkProcedures forks
                          incPC
              appendStep Ret
              nextCase 

            
              generateRecursively procs
              stmts xs
          
          Procedure n   -> do
              let params = getParams xs

              {- enter -}
              let strarg (Left (r,t))      = svtyp (dereference t) ++ " *" ++ r
                  strarg (Right (Var t n)) = prtyp t ++ " " ++ n
              let sig = concat ["act_", n, "_t *enter_", n]
              let args = [ "act_t *caller"
                         , "uint32_t priority"
                         , "uint8_t depth"
                         ] ++ map strarg params
              appendEnter $ Function sig args
              appendEnter $ Literal 4 $ concat [ "act_", n, "_t *act = "
                                             , "(act_", n, "_t *) enter(sizeof(act_", n, "_t), "
                                             , "step_", n, ", caller, priority, depth)"
                                             ]
              let assignParam (Left (r,t)) = do
                      appendEnter $ Literal 4 $ concat ["act->", r, " = ", r]
                  assignParam (Right (Var t n)) = do
                      appendEnter $ Literal 4 $ concat ["initialize_", prtyp t, "(&act->", n, ")"]
                      appendEnter $ Literal 4 $ concat ["assign_", prtyp t, "(&act->", n, ", act->priority, ", n, ")"]
              mapM_ assignParam params

              {- step -}
              appendStep $ Function ("void step_" ++ n) ["act_t *gen_act"]
              appendStep $ Literal 4 $ concat ["act_", n, "_t *act = (act_", n, "_t *) gen_act"]
              appendStep $ LiteralNoSemi 4 "switch (act->pc) {"
              nextCase
              stmts xs
          
          Argument _ n arg -> do
              {- struct -}
              case arg of
                  Left e      -> appendStruct $ FieldDec (expType e) n (Just " // Procedure argument")
                  Right (r,t) -> appendStruct $ FieldDec t n           (Just " // Procedure argument")
              stmts xs
 
          Result n -> do
              nw <- gets numwaits

              {- struct -}
              prependStruct [ LiteralNoSemi 0 "typedef struct {"
                            , LiteralNoSemi 4 "/* Generic procedure fields */"
                            , Literal 4       "ACTIVATION_RECORD_FIELDS"
                            , LiteralNoSemi 4 "/* Procedure specific fields */"
                            ]
              forM_ [1..nw] $ \i -> do
                  appendStruct $ Literal 4 $ "trigger_t trig" ++ show i
              appendStruct $ Literal 0 $ "} act_" ++ n ++ "_t"

              {- enter -}
              forM_ [1..nw] $ \i -> do
                  appendEnter $ Literal 4 $ "act->trig" ++ show i ++ ".act = (act_t *) act"
              appendEnter $ Literal 4 "return act"
              appendEnter $ Literal 0 "}"

              {- step -}
              appendStep Blank
              appendStep $ Leave n
        where
            file :: String -> String
            file str = reverse $ takeWhile (/= '/') (reverse str)

            -- | Generate a fresh label
            freshLabel :: TR String
            freshLabel = do
                st <- get
                put $ st { nextLabel = nextLabel st + 1 }
                return $ "L" ++ show (nextLabel st)
                        
            -- | Compile a program into a tuple that describes a fork
            compileFork :: [SSMStm] -> (String, [Either Reference SSMExp])
            compileFork ssm = let fun  = getFun $ head ssm
                                  args = getArgs ssm
                              in (fun, args)
                where
                    -- | Fetch the name of a procedure
                    getFun :: SSMStm -> String
                    getFun (Procedure n) = n
                    getFun _               = error "not a function"

                    -- | Fetch the values a function was applied to
                    getArgs :: [SSMStm] -> [Either Reference SSMExp]
                    getArgs (Procedure _:xs)               = getArgs xs
                    getArgs (Argument _ name (Left e):xs)  = Right e : getArgs xs
                    getArgs (Argument _ name (Right r):xs) = Left  r : getArgs xs
                    getArgs s                              = []

            -- | Fetch the parameters of a procedure, and the rest of the program
            getParams :: [SSMStm] -> [Either Reference SSMExp]
            getParams (Argument _ n (Left e):xs)      = Right (Var (expType e) n) : getParams xs
            getParams (Argument _ n (Right (r,t)):xs) = Left (n, t)               : getParams xs
            getParams _ = []

            {- | Take the programs that were forked and put them in the `toGenerate` list, if they
            have not already been generated. -}
            generateRecursively :: [SSM ()] -> TR ()
            generateRecursively xs = do
                forM_ xs $ \ssm -> do
                    let n = getProcedureName $ head $ runSSM ssm
                    gen <- gets generated
                    if n `elem` gen
                        then return ()
                        else modify $ \st -> st { toGenerate = ssm : toGenerate st}

            incPC :: TR ()
            incPC = do
                i <- gets ncase
                appendStep $ Literal 12 $ "act->pc = " ++ show i

            nextCase :: TR ()
            nextCase = do
                i <- gets ncase
                modify $ \st -> st { ncase = i + 1 }
                appendStep $ Case i

-- | Convert a sequence of IR values into (hopefully) valid C code
genCFromIR :: [IR] -> [Reference] -> Writer ([String], String) ()
genCFromIR [] _                   = return ()
genCFromIR (Blank:Blank:xs) lrefs = genCFromIR (Blank:xs) lrefs
genCFromIR (x:xs) lrefs           = case x of
    Function name args -> do let sig = name ++ "(" ++ intercalate ", " args ++ ")"
                             indent 0 $ sig ++ " {"

                             tell ([], sig ++ ";")
                             genCFromIR xs lrefs
    FieldDec ftyp fname c -> do let ct = svtyp ftyp
                                indent 4 $ concat [ct, fname, ";", fromMaybe "" c]
                                genCFromIR xs lrefs

    IfFalse notexp label -> do indent 12 $ "if (!(" ++ compLit notexp ++ ")) goto " ++ label ++ ";"
                               genCFromIR xs lrefs
    Label label      -> do indent 8 $ label ++ ":"
                           genCFromIR xs lrefs
    Goto label       -> do indent 12 $ "goto " ++ label ++ ";"
                           genCFromIR xs lrefs
    Case num         -> do indent 8 $ "case " ++ show num ++ ":"
                           genCFromIR xs lrefs
    Ret              -> do indent 12 "return;"
                           genCFromIR xs lrefs

    Initialize var      -> let st = simpleType var
                               v  = compVar var
                           in do indent 12 $ concat ["initialize_", st, "(", v, ");"]
                                 genCFromIR xs lrefs
    Assign var val      -> let st   = simpleType var
                               var' = compVar var
                               val' = compVal val
                           in do indent 12 $ concat ["assign_", st, "(", var', ", act->priority, ", val', ");"]
                                 genCFromIR xs lrefs
    Later ref delay val -> let st     = simpleType (Left ref)
                               var    = compVar (Left ref)
                               time   = compLit delay
                               newval = compLit val
                           in do indent 12 $ concat ["later_", st, "(", var, ", now + ", time, ", ", newval, ");"]
                                 genCFromIR xs lrefs
    EventOn ref var     -> let st      = simpleType (Right var)
                               ref'    = compVar (Left ref)
                               var'    = compVar (Right var)
                               eventon = concat ["event_on((sv_t *)", ref', ")"]
                           in do indent 12 $ concat ["assign_", st, "(", var', ", act->priority, ", eventon, ");"]
                                 genCFromIR xs lrefs

    Sensitize vars   -> do
        forM_ vars $ \(ref,trig) -> do
            let var = compVar (Left ref)
            indent 12 $ concat ["sensitize((sv_t *) ", var, ", &act->trig", show trig, ");"]
        genCFromIR xs lrefs
    Desensitize trig -> do indent 12 $ concat ["desensitize(&act->trig", show trig, ");"]
                           genCFromIR xs lrefs

    Call fun args    -> let args' = map compArg args
                            argstr = if not (null args')
                                       then [", "] ++ intersperse ", " args'
                                       else [""]
                        in do indent 12 $ concat (["call((act_t *) enter_", fun, "((act_t *) act, act->priority, act->depth"] ++ argstr ++ ["));"])
                              genCFromIR xs lrefs
    ForkProcedures procs -> do let new_depth = integerLogBase 2 (toInteger (length procs))
                               indent 12   "{"
                               indent 12 $ "uint8_t new_depth = act->depth - " ++ show new_depth ++ ";"
                               indent 12   "uint32_t pinc = 1 << new_depth;"
                               indent 12   "uint32_t new_priority = act->priority;"
                               let forks = intercalate ["new_priority += pinc;"] $ map compileFork procs
                               mapM_ (indent 12) forks
                               indent 12   "}"
                               genCFromIR xs lrefs
    Leave fun        -> do indent 8 $ concat ["leave((act_t *) act, sizeof(act_", fun, "_t));"]
                           indent 4 "}"
                           indent 0 "}"

    Blank               -> indent 0 "" >> genCFromIR xs lrefs
    Literal i lit       -> indent i (lit ++ ";") >> genCFromIR xs lrefs
    LiteralNoSemi i lit -> indent i lit >> genCFromIR xs lrefs
  where
      indent :: Int -> String -> Writer ([String], String) ()
      indent i str = tell ([replicate i ' ' ++ str], "")

      -- | Compile a Fork statement into the equivalent C statement
      compileFork :: (String, [Either Reference SSMExp]) -> [String]
      compileFork (fun, ar) = let f   = "fork_routine((act_t *) enter_" ++ fun ++ "( "
                                  lf  = length f
                                  ars = intercalate ('\n' : replicate (length f + 10) ' ' ++ ", ") args
                                in [f ++ ars ++ "));"]
        where
            args :: [String]
            args = ["(act_t *) act", "new_priority", "new_depth"] ++ map compArg ar

      -- | Compile an expression into a value
      compLit :: SSMExp -> String
      compLit e = case e of
          Var _ e        -> "act->" ++ e ++ ".value"
          Lit _ l        -> case l of
                              LInt i      -> if i >= 0 then show i else "(" ++ show i ++ ")"
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

      -- | Compile either a reference or a variable into the string that fetches it out of the struct
      compVar :: Either Reference SSMExp -> String
      compVar (Left ref@(r,_))  = if ref `elem` lrefs
                                    then "&act->" ++ r
                                    else "act->"  ++ r
      compVar (Right (Var t n)) = "&act->" ++ n
      compVar (Right e)         = error $ "not a variable: " ++ show e

      -- | Compile either a reference or an expression to their correspending values.
      compVal :: Either Reference SSMExp -> String
      compVal (Left ref@(r,_))  = if ref `elem` lrefs
                                    then "act->" ++ r ++ ".value"
                                    else "act->" ++ r ++ "->value"
      compVal (Right e)         = compLit e

      {- | Compile an argument to a function. We pass references as they are, but for literals
      we fetch them out of the sv-struct. -}
      compArg :: Either Reference SSMExp -> String
      compArg (Left ref@(r,_))  = if ref `elem` lrefs
                                    then "&act->" ++ r
                                    else "act->" ++ r
      compArg (Right (Var _ v)) = "act->" ++ v ++ ".value"
      compArg (Right e)         = compVal (Right e)

      -- | Print the 'simple type' of a value. E.g the underlying type of a Ref Int is an Int.
      simpleType :: Either Reference SSMExp -> String
      simpleType (Left (r,t)) = prtyp $ dereference t
      simpleType (Right e)    = prtyp $ expType e

generateMain :: [SSMStm] -> Maybe Int -> Writer [String] ()
generateMain ssm@(Procedure n:xs) d = do
    top_return
    indent 0 "void main(void) {"
    indent 4 "act_t top = { .step = top_return };"
    createrefs $ refs xs
    forkentrypoint ssm
    indent 4 "tick();"
    indent 4 "printf(\"now %lu eventqueuesize %d\\n\", now, event_queue_len);"
    maybe (return ())
          (\s -> indent 4 (concat ["int counter = ", show s, ";"])) d
    maybe (indent 4 "while(1) {")
          (\_ -> indent 4 "while(counter) {") d
    indent 8 "now = next_event_time();"
    indent 8 "if(now == NO_EVENT_SCHEDULED) {"
    indent 12 "break;"
    indent 8 "}"
    indent 8 "tick();"
    indent 8 "printf(\"now %lu eventqueuesize %d\\n\", now, event_queue_len);"
    maybe (return ())
          (\_ -> indent 8 "counter = counter - 1;") d
    indent 4 "}"
    printrefs $ refs xs
    indent 0 "}"
  where
      indent :: Int -> String -> Writer [String] ()
      indent i str = tell [replicate i ' ' ++ str]

      refs :: [SSMStm] -> [Reference]
      refs (Argument _ n (Right (_,t)):xs) = (n,t) : refs xs
      refs (Argument _ n (Left _):xs)      = refs xs
      refs _                               = []

      createrefs :: [Reference] -> Writer [String] ()
      createrefs refs = do
          indent 4 "// set up input references"
          forM_ refs $ \(r,t) -> do
              let sv = svtyp (dereference t)
              let pr = prtyp (dereference t)
              indent 4 $ concat [sv, " ", r, ";"]
              indent 4 $ concat ["initialize_", pr, "(&", r,");"]
              indent 4 $ concat [r, ".value = ", baseval t, ";"]
        where
            baseval :: Type -> String
            baseval TInt    = "0"
            baseval TBool   = "false"
            baseval (Ref t) = baseval t
              
      forkentrypoint :: [SSMStm] -> Writer [String] ()
      forkentrypoint (Procedure n:xs) = do
          let args = vals xs
          indent 4 $ concat ["fork_routine((act_t *) enter_"
                            , n
                            , "(&top, PRIORITY_AT_ROOT, DEPTH_AT_ROOT"
                            , if null args then "" else ", "
                            , intercalate ", " (vals xs)
                            , "));"]
        where
            vals :: [SSMStm] -> [String]
            vals (Argument _ n (Left e):xs)  = compExp e : vals xs
            vals (Argument _ n (Right _):xs) = ("&" ++ n) : vals xs
            vals _                           = []

            compExp :: SSMExp -> String
            compExp (Var _ _)            = error "can not apply main procedure to expression variables"
            compExp (Lit _ (LInt i))     = show i
            compExp (Lit _ (LBool b))    = if b then "true" else "false"
            compExp (UOp _ e Neg)        = "(-" ++ compExp e ++ ")"
            compExp (BOp _ e1 e2 OPlus)  = "(" ++ compExp e1 ++ ") + (" ++ compExp e2 ++ ")"
            compExp (BOp _ e1 e2 OMinus) = "(" ++ compExp e1 ++ ") - (" ++ compExp e2 ++ ")"
            compExp (BOp _ e1 e2 OTimes) = "(" ++ compExp e1 ++ ") * (" ++ compExp e2 ++ ")"
            compExp (BOp _ e1 e2 OLT)    = "(" ++ compExp e1 ++ ") < (" ++ compExp e2 ++ ")"
            compExp (BOp _ e1 e2 OEQ)    = "(" ++ compExp e1 ++ ") == (" ++ compExp e2 ++ ")"

      printrefs :: [Reference] -> Writer [String] ()
      printrefs xs = do
          forM_ xs $ \(r,t) -> do
              indent 4 $ concat ["printf(\"", r, " ", formatter (dereference t), "\\n\", ", r, ".value);"]
        where
            formatter :: Type -> String
            formatter TInt    = "%u"
            formatter TBool   = "%d"
            formatter (Ref t) = error "can not print pointer yet"

      top_return :: Writer [String] ()
      top_return = do
          indent 0 "void top_return(act_t *act) {"
          indent 4 "return;"
          indent 0 "}"
          indent 0 ""
    
-- | Print a base type
prtyp :: Type -> String
prtyp TInt  = "int"
prtyp TBool = "bool"
prtyp t     = error $ "not a simple type: " ++ show t

-- | Convert a type to the string representing the sv-type
svtyp :: Type -> String
svtyp t = if isReference t
    then concat ["sv_", prtyp (dereference t), "_t *"]
    else concat ["sv_", prtyp t, "_t "]