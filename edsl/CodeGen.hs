module CodeGen where

import Control.Monad.State
import Control.Monad.Writer
import GHC.Float
import Data.List

import Core

{-
TODO:
  * Now we perform three passes over the program, one for the struct, enter function and step function.
    This could perhaps be changed to a version that only performs one pass.
-}

-- | One value of this datatype corresponds to one statement/line in the C code
data IR = Function String [String]

        -- Struct management
        | TypedefStruct            
        | FieldDec Type String
        | CloseTypeDef String
    
        | If2 SSMExp String -- if !exp goto label
        | Label String      -- label
        | Goto String       -- unconditional jump
        | Switch String     -- switch(-the string-) {
        | Case Int          -- case -the int-:
        | Return2

        -- Methods from the SSM runtime
        | Initialize (Either Reference SSMExp) SSMExp                 -- Initialize a variable
        | Assign (Either Reference SSMExp) (Either Reference SSMExp)  -- Assign to variables in the struct
        | Later Reference SSMExp SSMExp                               -- Delayed assignment
        | EventOn Reference SSMExp                                    -- Has the reference been written

        | Sensitize [(Reference, Int)] -- [(reference, triggernumber)]
        | Desensitize Int              -- Stop waiting for a variable

        | Call String  [Either Reference SSMExp]       -- (Procedure, arguments)
        | Fork2 [(String,  [Either Reference SSMExp])] -- [(Procedure, arguments)]
        | Leave String                                 -- Name of the procedure

        -- Memory management
        | Malloc String Type -- Variable name and type
        | Free Reference     -- Variable name

        | Blank              -- Blank linkes for readability
        | Literal Int String -- Verbatim code
  deriving Show

data TRState = TRState { nextLabel  :: Int         -- ^ Counter to generate fresh labels
                       , ncase      :: Int         -- ^ Which number has the next case?
                       , numwaits   :: Int         -- ^ The size of the widest wait
                       , localrefs  :: [Reference] -- ^ Local references declared with var
                       , generated  :: [String]    -- ^ Which procedures are already generated?
                       , toGenerate :: [SSM ()]    -- ^ Which remain to be generated?
                       }

type TR a = StateT TRState (Writer [IR]) a

-- | Main compiler entrypoint. Will output a C-file as a single string.
compile :: SSM () -> String
compile ssm = let wr     = evalStateT generateProcedures (TRState 0 0 0 [] [] [ssm])
                  (a, _) = runWriter wr
              in unlines a

-- | Return a list of all triples (struct, enter, step)
generateProcedures :: TR [String]
generateProcedures = do
    st <- gets toGenerate
    -- If there are no procedures left to generate C code for the function terminates
    if null st
        then return []
        else do
            -- Otherwise the procedure specific state is reset and the next procedure is fetched
            resetState
            gen <- gets generated
            let (toc, rest) = (head st, tail st)
            {- If we have not already generated code for this procedure we will do so, otherwise
            we will forget about the procedure and recursively call generateProcedures. -}
            if name toc `elem` gen
                then do modify $ \st -> st { toGenerate = rest}
                        generateProcedures
                else do -- Remove the procedure to generate code for from the toGenerate list
                        modify $ \state -> state { toGenerate = tail (toGenerate state)}
                        tri  <- ssmToC toc
                        {- After code has been generated for the procedure we add its name
                        to the list of generated procedures. -}
                        modify $ \state -> state { generated  = name toc : generated state}
                        rest <- generateProcedures
                        return $ rest ++ tri
  where
      -- | Get the name of a procedure
      name :: SSM () -> String
      name (Procedure n _ _) = n

      -- | Reset the procedure specific parts of the state before generating C for the next procedure
      resetState :: TR ()
      resetState = modify $ \st -> st { nextLabel = 0, ncase = 0, numwaits = 0, localrefs = []}

-- | Take a single SSM program and generate a struct, enter and step function.
ssmToC :: SSM () -> TR [String]
ssmToC ssm = do
    -- Generate the IR instructions that represent the struct, enter and step function
    struct <- censor (const []) $ snd <$> listen (genIRStruct ssm)
    enter  <- censor (const []) $ snd <$> listen (genIREnter ssm)
    step   <- censor (const []) $ snd <$> listen (genIRStep ssm)

    {- Generate C code from the IR instructions that were emitted. The step function needs to
    be converted to a version that returns at blocking points before C can be generated. -}
    let struct' = execWriter (genCFromIR struct)
    let enter'  = execWriter (genCFromIR enter)
    let step'   = execWriter (genCFromIR ((concat . wrapInSwitch . cases) step))

    return [unlines struct', unlines enter', unlines step']
  where
      -- | This function takes a program and splits it up at points where the code should block.
      cases :: [IR] -> [[IR]]
      cases [] = []
      cases xs = case myTakeWhile pred xs of
          ([], []) -> []
          (x, xs') -> x : cases xs'
        where
            -- | Predicate to know when to close a case. A case should be closed on a blocking call.
            pred :: IR -> Bool
            pred (Call _ _)      = True
            pred (Fork2 _)       = True
            pred (Sensitize _ )  = True
            pred _               = False

            -- | Like the normal takeWhile, but also includes the first element that fails the predicate
            myTakeWhile :: (a -> Bool) -> [a] -> ([a], [a])
            myTakeWhile p [] = ([], [])
            myTakeWhile p (x:xs) = if p x
                then ([x], xs)
                else let (x', xs') = myTakeWhile p xs
                     in (x:x', xs')

      {- | This function takes a list of blocks and transforms them into a big switch statement where
      the blocks become individual cases. -}
      wrapInSwitch :: [[IR]] -> [[IR]]
      wrapInSwitch xs = rewriteHead y : ys
        where
            wcase i = Case i
            incPC i = Literal 12 $ "rar->pc = " ++ show (i + 1)
            {- before every wakeup we insert a new case and after the statements we must increase
            the program counter and then return. -}
            (y:ys) = map (\(c,i) -> concat [[wcase i], c, [incPC i, Return2, Blank]]) (zip xs [0..])

            {- The first case in a procedure must be preceeded by the method signature,
            the type casting of the rar variable and a switch statement. -}
            rewriteHead :: [IR] -> [IR]
            rewriteHead xs = concat [[xs !! 1], [xs !! 2], [Switch "rar->pc"], [Case 0], drop 3 xs]

-- | Generate the struct for a procedure.
genIRStruct :: SSM () -> TR ()
genIRStruct ssm@(Procedure n _ _) = do
    -- Here we declare the generic fields in the struct
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
    -- Here we declare the procedure specific fields
    go ssm
    -- Here we declare how many triggers we need
    triggers
    tell [CloseTypeDef n]
  where
      -- | Traverse the program and see which fields need to exist in the struct
      go :: SSM () -> TR ()
      go ssm = case ssm of
        (Return x)                     -> return ()
        -- Local references will exist in the struct, so we declare one.
        (NewRef (Just (_,n)) e k)      -> do let ref = (n, mkReference (expType e))
                                             tell [FieldDec (mkReference (expType e)) n]
                                             go $ k ref
        (SetRef r e k)                 -> go $ k ()
        (SetLocal e v k)               -> go $ k ()
        {- The result of reading a reference will reside in a local variable that exists in
        the struct, so we declare one. -}
        (GetRef (r, t) (Just (_,n)) k) -> do let var = Var (dereference t) n
                                             tell [FieldDec (dereference t) n]
                                             go $ k var
        (If c thn (Just els) k)        -> go $ thn >> els >> k ()
        (If c thn Nothing k)           -> go $ thn >> k ()
        (While c bdy k)                -> go $ bdy >> k ()
        (After e r v k)                -> go $ k ()
        {- The result of checking if a variable was written in this instant will reside in
        a variable in the struct, so we declare one. -}
        (Changed r (Just (_,n)) k)     -> do let var = Var TBool n
                                             tell [FieldDec TBool n]
                                             go $ k var
        -- To know how many triggers to generate we need to know the size of the widest wait
        (Wait vars k)                  -> do modify $ \st -> st { numwaits = max (numwaits st) (length vars)}
                                             go $ k ()
        (Fork procs k)                 -> go $ k ()
        (Procedure n _ k)              -> go $ k ()
        -- We look at the arguments to this procedure and generate field declarations for them
        (Argument _ n (Left e) k)      -> do tell [FieldDec (expType e) n]
                                             go $ k ()
        (Argument _ n (Right (r,t)) k) -> do tell [FieldDec t n]
                                             go $ k ()
        (Result n r k)                 -> go $ k ()

      -- | Declare the triggers. We declare as many as the size of the widest wait.
      triggers :: TR ()
      triggers = do
          num <- gets numwaits
          mapM_ (\i -> tell [Literal 4 ("trigger_t trig" ++ show i)]) [1..num]

-- | Generate the enter function for a procedure
genIREnter :: SSM () -> TR ()
genIREnter ssm = case ssm of
    (Return x)                     -> return ()
    -- When creating a new local reference the enter function will allocate memory for it
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
    (Wait vars k)                  -> genIREnter $ k ()
    (Fork procs k)                 -> genIREnter $ k ()
    {- Here we do the bulk of the work while generating the enter function. We generate the function
    signature, the call to the generic enter function, assign the procedure specific parameters
    and initialise the triggers. -}
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
    -- We must close the scope of the enter function before we are done generating it
    (Result n r k)                 -> do tell [Literal 0 "}"]
                                         genIREnter $ k ()
  where
      {- | Fetch the procedure specific arguments of the enter function. They are combined
      with their type for easier manipulation when generating code. -}
      getParams :: SSM () -> [String]
      getParams (Argument _ name (Left e) k)      =
          (prtyp (expType e) ++ " " ++ name)       : getParams (k ())  
      getParams (Argument _ name (Right (r,t)) k) =
          (cvtype (dereference t) ++ " *" ++ name) : getParams (k ())
      getParams s                                 = []

      {- | We need to assign the value of the arguments, if any, to the corresponding fields
      in the struct. -}
      assignParams :: SSM () -> TR ()
      assignParams (Argument _ name (Left e) k)      = do
          tell [Literal 4 (concat ["initialise_", prtyp (expType e), "(&rar->", name, ", ", name, ")"])]
          assignParams $ k ()  
      assignParams (Argument _ name (Right (r,t)) k) = do
          tell [Literal 4 ("rar->" ++ name ++ " = " ++ name)]
          assignParams $ k ()
      assignParams s = return ()

      -- | Initialize all the triggers. The triggers need to be linked to the threads rar.
      initTriggers :: TR ()
      initTriggers = do
          nw <- gets numwaits
          mapM_ (\i -> tell [Literal 4 ("rar->trig" ++ show i ++ ".rar = (rar_t *) rar")]) [1..nw]

      -- | Convert a type to it's cv-version
      cvtype :: Type -> String
      cvtype t = "cv_" ++ prtyp t ++ "_t"

      -- | Print a base type
      prtyp :: Type -> String
      prtyp TInt = "int"
      prtyp TBool = "bool"
      prtyp t     = error $ "not a simple type: " ++ show t

-- | Generate the step function for a procedure.
genIRStep :: SSM () -> TR ()
genIRStep ssm = case ssm of
    (Return x)                     -> do return ()
    {- The reference being created will reside in the struct, so what is left is
    essentially only to initialize it to the desired value. -}
    (NewRef (Just (_,n)) e k)      -> do let ref = (n, mkReference (expType e))
                                         tell [Initialize (Left ref) e]
                                         genIRStep $ k ref
    {- Setting the value of a reference or a loval reference is very straight forward.
    We issue an assignment statement that assigns the value to the variable. -}
    (SetRef r e k)                 -> do tell [Assign (Left r) (Right e)]
                                         genIRStep $ k ()
    (SetLocal e v k)               -> do tell [Assign (Right e) (Right v)]
                                         genIRStep $ k ()
    {- The result of fetching the value of a reference will be stored in a local reference. This
    variable will exist in the struct of this procedure, so we simply issue an assignment from
    the reference to the local reference in the struct. -}
    (GetRef (r, t) (Just (_,n)) k) -> do let var = Var (dereference t) n
                                         tell [Assign (Right var) (Left (r,t))]
                                         genIRStep $ k var
    {- To avoid scoping and stuff we compile IF statements to code that uses labels and goto's.
    This makes code generation easier as we can always emit a new case without having to
    bother about closing scopes etc. -}
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
    {- Issuing delayed assignments are very straight forward, we don't have to do anything other than
    convert the instruction to the IR representation of it. -}
    (After e r v k)                -> do tell [Later r e v]
                                         genIRStep $ k ()
    {- When checking if a reference has been written to in this instance in time, the result will be
    stored in a local variable. This variable will reside in the struct, so we need to make sure
    that we write the result to that variable. The `EventOn` constructor takes a variable as its second
    argument, signifying the variable. We send a complete expression rather than just a string so that
    we will also get the type information stored in the expression. -}
    (Changed r (Just (_,n)) k)     -> do let var = Var (dereference (snd r)) n
                                         tell [EventOn r var]
                                         genIRStep $ k var
    {- To wait for one or more variables we need to create a `Sensitize` value, where we pair up
    variables and the triggers they will wait on. The statement following this one will be executed
    when the thread has woken up, so we make sure to desensitize the triggers before we proceed. -}
    (Wait vars k)                  -> do tell [Sensitize (zip vars [1..])]
                                         mapM_ (\(_,i) -> tell [Desensitize i]) (zip vars [1..])
                                         genIRStep $ k ()
    {- When we compile a fork statement we must not forget to instruct the code generator that
    we need to generate code for the forked procedures. This is done by the `forkProcs` function. -}
    (Fork procs k)                 -> do forks <- mapM compileFork procs
                                         if length forks == 1
                                             then tell $ map (uncurry Call) forks
                                             else tell [Fork2 forks]
                                         forkProcs procs
                                         genIRStep $ k ()
    {- To generate code for this value we emit the two 'constant' lines that begin each step function,
    and then proceed with the procedure specific instruction. -}
    (Procedure n _ k)              -> do let (_, rest) = getParams $ k ()
                                         tell [Function ("void step_" ++ n) ["rar_t *gen_rar"]]
                                         tell [Literal 4 $ concat ["rar_", n, "_t *rar = (rar_", n, "_t *) gen_rar"]]
                                         genIRStep rest
    {- When we exit a step function we need to free any local references that we might have allocated,
    and then call the generic exit function. -}
    (Result n r k)                 -> do tell [Blank]
                                         lrefs <- gets localrefs
                                         mapM_ (\ref -> tell [Free ref]) lrefs
                                         tell [Leave n]
  where
      -- | Fetch the parameters of a procedure, and the rest of the program
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

      -- | Compile a program into a tuple that describes a fork
      compileFork :: SSM () -> TR (String, [Either Reference SSMExp])
      compileFork ssm = let fun  = getFun ssm
                            args = getArgs ssm
                        in return (fun, args)
        where
            -- | Fetch the name of a procedure
            getFun :: SSM () -> String
            getFun (Procedure n _ _) = n
            getFun _                 = error "not a function"

            -- | Fetch the values a function was applied to
            getArgs :: SSM () -> [Either Reference SSMExp]
            getArgs (Procedure _ _ k)             = getArgs $ k ()
            getArgs (Argument _ name (Left e) k)  = Right e : getArgs (k ())
            getArgs (Argument _ name (Right r) k) = Left  r : getArgs (k ())
            getArgs s                             = []
    
      {- | Take the programs that were forked and put them in the `toGenerate` list, if they
      have not already been generated. -}
      forkProcs :: [SSM ()] -> TR ()
      forkProcs xs = do
          forM_ xs $ \ssm@(Procedure n _ _) -> do
              gen <- gets generated
              if n `elem` gen
                  then return ()
                  else modify $ \st -> st { toGenerate = ssm : toGenerate st}

      -- | Generate a fresh label
      freshLabel :: TR String
      freshLabel = do
          st <- get
          put $ st { nextLabel = nextLabel st + 1 }
          return $ "L" ++ show (nextLabel st)

-- | Convert a sequence of IR values into (hopefully) valid C code
genCFromIR :: [IR] -> Writer [String] ()
genCFromIR []               = return ()
genCFromIR (Blank:Blank:xs) = genCFromIR (Blank:xs)
genCFromIR (x:xs)           = case x of
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

    Sensitize vars   -> do
        forM_ vars $ \(ref,trig) -> do
            let var = compVar (Left ref)
            indent 12 $ concat ["sensitize((cv_t *) ", var, ", &rar->trig", show trig, ");"]
        genCFromIR xs
    Desensitize trig -> do indent 12 $ concat ["desensitize(&rar->trig", show trig, ");"]
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

    Malloc var typ -> do let st  = prtyp typ
                         let ctp = cvtyp (mkReference typ)
                         let ct  = cvtyp typ
                         indent 4 $ concat ["rar->", var, " = (", ctp, ") malloc(sizeof(", init ct, "));"]
                         genCFromIR xs
    Free (var, _)  -> do indent 8 $ concat ["free(", var, ");"]
                         genCFromIR xs

    Blank -> indent 0 "" >> genCFromIR xs
    Literal i lit -> indent i (lit ++ ";") >> genCFromIR xs
  where
      indent :: Int -> String -> Writer [String] ()
      indent i str = tell [replicate i ' ' ++ str]

      -- | Compile a Fork statement into the equivalent C statement
      compileFork :: (String, [Either Reference SSMExp]) -> [String]
      compileFork (fun, ar) = let f  = "fork((rar_t *) enter_" ++ fun ++ "( "
                                  lf = length f
                                  ars = intercalate ('\n' : replicate (length f + 10) ' ' ++ ", ") args
                                in [f ++ ars ++ "));"]
        where
            args :: [String]
            args = ["(rar_t *) rar", "new_priority", "new_depth"] ++ map compArg ar

      -- | Compile an expression into a value
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

      -- | Compile either a reference or a variable into the string that fetches it out of the struct
      compVar :: Either Reference SSMExp -> String
      compVar (Left (r,_))      = "rar->"  ++ r
      compVar (Right (Var _ n)) = "&rar->" ++ n
      compVar (Right e)         = error $ "not a variable: " ++ show e

      -- | Compile either a reference or an expression to their correspending values.
      compVal :: Either Reference SSMExp -> String
      compVal (Left (r,_))      = "rar->" ++ r ++ "->value"
      compVal (Right e)         = compLit e

      {- | Compile an argument to a function. We pass references as they are, but for literals
      we fetch them out of the cv-struct. -}
      compArg :: Either Reference SSMExp -> String
      compArg (Left (r,_))      = "rar->" ++ r
      compArg (Right (Var _ v)) = "rar->" ++ v ++ ".value"
      compArg (Right e)         = compVal (Right e)

      -- | Print the 'simple type' of a value. E.g the underlying type of a Ref Int is an Int.
      simpleType :: Either Reference SSMExp -> String
      simpleType (Left (r,t)) = prtyp $ dereference t
      simpleType (Right e)    = prtyp $ expType e

      -- | Print a base type
      prtyp :: Type -> String
      prtyp TInt  = "int"
      prtyp TBool = "bool"
      prtyp t     = error $ "not a simple type: " ++ show t

      -- | Convert a type to the string representing the cv-type
      cvtyp :: Type -> String
      cvtyp t = if isReference t
          then concat ["cv_", prtyp (dereference t), "_t *"]
          else concat ["cv_", prtyp t, "_t "]