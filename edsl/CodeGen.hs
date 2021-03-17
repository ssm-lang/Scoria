module CodeGen where

import Control.Monad.State
import Control.Monad.Writer
import GHC.Float
import Data.List
import Data.Maybe

import Core

{-
TODO:
  * Now we perform three passes over the program, one for the struct, enter function and step function.
    This could perhaps be changed to a version that only performs one pass.
-}

-- | One value of this datatype corresponds to one statement/line in the C code
data IR = Function String [String]

        | FieldDec Type String (Maybe String) -- Last string is a comment
    
        | IfFalse SSMExp String -- if !exp goto label
        | Label String      -- label
        | Goto String       -- unconditional jump
        | Switch String     -- switch(-the string-) {
        | Case Int          -- case -the int-:
        | Ret
        | ClosingBracket Int

        -- Methods from the SSM runtime
        | Initialize (Either Reference SSMExp)                        -- Initialize a variable
        | Assign (Either Reference SSMExp) (Either Reference SSMExp)  -- Assign to variables in the struct
        | Later Reference SSMExp SSMExp                               -- Delayed assignment
        | EventOn Reference SSMExp                                    -- Has the reference been written

        | Sensitize [(Reference, Int)] -- [(reference, triggernumber)]
        | Desensitize Int              -- Stop waiting for a variable

        | Call String  [Either Reference SSMExp]       -- (Procedure, arguments)
        | ForkProcedures [(String,  [Either Reference SSMExp])] -- [(Procedure, arguments)]
        | Leave String                                 -- Name of the procedure

        -- Memory management
        | Malloc String Type -- Variable name and type
        | Free Reference     -- Variable name

        | Blank              -- Blank linkes for readability
        | Literal Int String -- Verbatim code
        | LiteralNoSemi Int String
  deriving Show

data TRState = TRState { nextLabel  :: Int         -- ^ Counter to generate fresh labels
                       , ncase      :: Int         -- ^ Which number has the next case?
                       , numwaits   :: Int         -- ^ The size of the widest wait
                       , localrefs  :: [Reference] -- ^ Local references declared with var
                       , code       :: ([IR], [IR], [IR])
                       , generated  :: [String]    -- ^ Which procedures are already generated?
                       , toGenerate :: [SSM ()]    -- ^ Which remain to be generated?
                       }

type TR a = State TRState a

-- | Main compiler entrypoint. Will output a C-file as a single string.
compile :: SSM () -> String
compile ssm = let methods = evalState generateProcedures (TRState 0 0 0 [] ([], [], []) [] [ssm])
              in unlines $ [ "#include \"peng-platform.h\""
                           , "#include \"peng.h\""
                           , "#include <stdio.h>"
                           , ""
                           ] ++ methods

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
      resetState = modify $ \st -> st { nextLabel = 0
                                      , ncase     = 0
                                      , numwaits  = 0
                                      , localrefs = []
                                      , code      = ([], [], [])}

      -- | Take a single SSM program and generate a struct, enter and step function.
      ssmToC :: SSM () -> TR [String]
      ssmToC ssm = do
          genIR ssm
          (struct, enter, step) <- gets code

          {- Generate C code from the IR instructions that were emitted. The step function needs to
          be converted to a version that returns at blocking points before C can be generated. -}
          let (struct', _)  = execWriter (genCFromIR struct)
          let (enter',  p2) = execWriter (genCFromIR enter)
          let (step',   p3) = execWriter (genCFromIR step)

          return [unlines struct', p2, p3, "", unlines enter', unlines step']

genIR :: SSM () -> TR ()
genIR ssm = stmts ssm
  where
      appendStruct :: IR -> TR ()
      appendStruct ir = modify $ \st -> st {code = code st <> ([ir], [], [])}

      prependStruct :: [IR] -> TR ()
      prependStruct ir = modify $ \st -> st {code = (ir, [], []) <> code st}

      appendEnter :: IR -> TR ()
      appendEnter ir = modify $ \st -> st {code = code st <> ([], [ir], [])}

      appendStep :: IR -> TR ()
      appendStep ir = modify $ \st -> st {code = code st <> ([], [], [ir])}

      stmts :: SSM () -> TR ()
      stmts ssm = case ssm of
          (Return x) -> return ()

          (NewRef (Just ((f,x,y),n)) e k) -> do
              let reftype = mkReference $ expType e
              let ref     = (n, reftype)
              let comment = concat [" // Declared at (", show x, ", ", show y, ") in file ", file f]
              modify $ \st -> st {localrefs = localrefs st ++ [ref]}

              appendStruct $ FieldDec reftype n (Just comment)

              appendEnter $ Malloc n (expType e)

              appendStep $ Initialize (Left ref)
              appendStep $ Assign (Left ref) (Right e)

              stmts $ k ref

          (SetRef r e k) -> do
               appendStep $ Assign (Left r) (Right e)
               stmts $ k ()
          
          (SetLocal e v k) -> do
              appendStep $ Assign (Right e) (Right v)
              stmts $ k ()
          
          (GetRef (r, t) (Just ((f,x,y),n)) k) -> do
              let vartyp  = dereference t
              let var     = Var vartyp n
              let comment = concat [" // Declared at (", show x, ", ", show y, ") in file ", file f]

              appendStruct $ FieldDec vartyp n (Just comment)

              appendStep $ Assign (Right var) (Left (r,t))

              stmts $ k var

          (If c thn (Just els) k) -> do
              l1 <- freshLabel
              l2 <- freshLabel

              appendStep $ IfFalse c l1
              stmts thn
              appendStep $ Goto l2
              appendStep   Blank
              appendStep $ Label l1
              stmts els
              appendStep   Blank
              appendStep $ Label l2
              
              stmts $ k ()

          
          (If c thn Nothing k) -> do
              l <- freshLabel
              
              appendStep $ IfFalse c l
              stmts thn
              appendStep   Blank
              appendStep $ Label l

              stmts $ k ()

          (While c bdy k) -> undefined -- TODO
          
          (After e r v k) -> do
              appendStep $ Later r e v
              stmts $ k ()
          
          (Changed r (Just ((f,x,y),n)) k) -> do
              let var = Var TBool n
              let comment = concat [" // Declared at (", show x, ", ", show y, " in file ", file f]

              appendStruct $ FieldDec TBool n (Just comment)
              
              appendStep $ EventOn r var

              stmts $ k var
          
          (Wait vars k) -> do
              modify $ \st -> st { numwaits = max (numwaits st) (length vars)}

              appendStep $ Sensitize $ zip vars [1..]

              stmts $ k ()
          
          (Fork procs k) -> do
              let forks = map compileFork procs

              if length procs == 1
                  then appendStep $ uncurry Call (head forks)
                  else appendStep $ ForkProcedures forks
            
              generateRecursively procs

              stmts $ k ()
          
          (Procedure n _ k) -> do
              let params = getParams $ k ()

              {- enter -}
              let strarg (Left (r,t))      = svtype (dereference t) ++ " *" ++ r
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

              stmts $ k ()
          
          (Argument _ n arg k) -> do
              case arg of
                  Left e      -> appendStruct $ FieldDec (expType e) n (Just " // Procedure argument")
                  Right (r,t) -> appendStruct $ FieldDec t n           (Just " // Procedure argument")
            
              stmts $ k ()
 
          (Result n r k) -> do
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
              lrefs <- gets localrefs
              mapM_ (appendStep . Free) lrefs
              appendStep $ Leave n

              (struct, enter, step) <- gets code
              modify $ \st -> st { code = (struct, enter, (concat . wrapInSwitch . cases) step)}

              stmts $ k ()
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
            compileFork :: SSM () -> (String, [Either Reference SSMExp])
            compileFork ssm = let fun  = getFun ssm
                                  args = getArgs ssm
                              in (fun, args)
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
            
            -- | Fetch the parameters of a procedure, and the rest of the program
            getParams :: SSM () -> [Either Reference SSMExp]
            getParams (Argument _ n (Left e) k)      = Right (Var (expType e) n) : getParams (k ())
            getParams (Argument _ n (Right (r,t)) k) = Left (n, t)               : getParams (k ())
            getParams _ = []

            {- | Take the programs that were forked and put them in the `toGenerate` list, if they
            have not already been generated. -}
            generateRecursively :: [SSM ()] -> TR ()
            generateRecursively xs = do
                forM_ xs $ \ssm@(Procedure n _ _) -> do
                    gen <- gets generated
                    if n `elem` gen
                        then return ()
                        else modify $ \st -> st { toGenerate = ssm : toGenerate st}
            
            -- | Convert a type to it's sv-version
            svtype :: Type -> String
            svtype t = "sv_" ++ prtyp t ++ "_t"

            -- | Print a base type
            prtyp :: Type -> String
            prtyp TInt = "int"
            prtyp TBool = "bool"
            prtyp t     = error $ "not a simple type: " ++ show t
  
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
              pred (ForkProcedures _)       = True
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
              incPC i = Literal 12 $ "act->pc = " ++ show (i + 1)
              {- before every wakeup we insert a new case and after the statements we must increase
              the program counter and then return. -}
              (y:ys) = map (\(c,i) -> concat [[wcase i], c, [incPC i, Ret, Blank]]) (zip xs [0..])

              {- The first case in a procedure must be preceeded by the method signature,
              the type casting of the act variable and a switch statement. -}
              rewriteHead :: [IR] -> [IR]
              rewriteHead xs = concat [[xs !! 1], [xs !! 2], [Switch "act->pc"], [Case 0], drop 3 xs]


-- | Convert a sequence of IR values into (hopefully) valid C code
genCFromIR :: [IR] -> Writer ([String], String) ()
genCFromIR []               = return ()
genCFromIR (Blank:Blank:xs) = genCFromIR (Blank:xs)
genCFromIR (x:xs)           = case x of
    Function name args -> do let sig = name ++ "(" ++ intercalate ", " args ++ ")"
                             indent 0 $ sig ++ " {"

                             tell ([], sig ++ ";")
                             genCFromIR xs
    FieldDec ftyp fname c -> do let ct = svtyp ftyp
                                indent 4 $ concat [ct, fname, ";", fromMaybe "" c]
                                genCFromIR xs
    
    IfFalse notexp label -> do indent 12 $ "if (!(" ++ compLit notexp ++ ")) goto " ++ label ++ ";"
                               genCFromIR xs
    Label label      -> do indent 8 $ label ++ ":"
                           genCFromIR xs
    Goto label       -> do indent 12 $ "goto " ++ label ++ ";"
                           genCFromIR xs
    Switch scrut     -> do indent 4 $ "switch(" ++ scrut ++ ") {"
                           genCFromIR xs
    Case num         -> do indent 8 $ "case " ++ show num ++ ":"
                           genCFromIR xs
    Ret              -> do indent 12 "return;"
                           genCFromIR xs
    ClosingBracket i -> do indent i "}"
                           genCFromIR xs

    Initialize var      -> let st = simpleType var
                               v  = compVar var
                           in do indent 12 $ concat ["initialize_", st, "(", v, ");"]
                                 genCFromIR xs
    Assign var val      -> let st   = simpleType var
                               var' = compVar var
                               val' = compVal val
                           in do indent 12 $ concat ["assign_", st, "(", var', ", act->priority, ", val', ");"]
                                 genCFromIR xs
    Later ref delay val -> let st     = simpleType (Left ref)
                               var    = compVar (Left ref)
                               time   = compLit delay
                               newval = compLit val
                           in do indent 12 $ concat ["later_", st, "(", var, ", now + ", time, ", ", newval, ");"]
                                 genCFromIR xs
    EventOn ref var     -> let st      = simpleType (Right var)
                               ref'    = compVar (Left ref)
                               var'    = compVar (Right var)
                               eventon = concat ["event_on((sv_t *)", ref', ")"]
                           in do indent 12 $ concat ["assign_", st, "(", var', ", act->priority, ", eventon, ");"]
                                 genCFromIR xs

    Sensitize vars   -> do
        forM_ vars $ \(ref,trig) -> do
            let var = compVar (Left ref)
            indent 12 $ concat ["sensitize((sv_t *) ", var, ", &act->trig", show trig, ");"]
        genCFromIR xs
    Desensitize trig -> do indent 12 $ concat ["desensitize(&act->trig", show trig, ");"]
                           genCFromIR xs

    Call fun args    -> let args' = map compArg args
                        in do indent 12 $ concat (["call((act_t *) enter_", fun, "((act_t *) act, act->priority, act->depth, "] ++ intersperse ", " args' ++ ["));"])
                              genCFromIR xs
    ForkProcedures procs -> do let new_depth = integerLogBase 2 (toInteger (length procs))
                               indent 12   "{"
                               indent 12 $ "uint8_t new_depth = act->depth - " ++ show new_depth ++ ";"
                               indent 12   "uint32_t pinc = 1 << new_depth;"
                               indent 12   "uint32_t new_priority = act->priority;"
                               let forks = intercalate ["new_priority += pinc;"] $ map compileFork procs
                               mapM_ (indent 12) forks
                               indent 12   "}"
                               genCFromIR xs
    Leave fun        -> do indent 8 $ concat ["leave((act_t *) act, sizeof(act_", fun, "_t));"]
                           indent 4 "}"
                           indent 0 "}"

    Malloc var typ -> do let st  = prtyp typ
                         let ctp = svtyp (mkReference typ)
                         let ct  = svtyp typ
                         indent 4 $ concat ["act->", var, " = (", ctp, ") malloc(sizeof(", init ct, "));"]
                         genCFromIR xs
    Free (var, _)  -> do indent 8 $ concat ["free(act->", var, ");"]
                         genCFromIR xs

    Blank -> indent 0 "" >> genCFromIR xs
    Literal i lit -> indent i (lit ++ ";") >> genCFromIR xs
    LiteralNoSemi i lit -> indent i lit >> genCFromIR xs
  where
      indent :: Int -> String -> Writer ([String], String) ()
      indent i str = tell ([replicate i ' ' ++ str], "")

      -- | Compile a Fork statement into the equivalent C statement
      compileFork :: (String, [Either Reference SSMExp]) -> [String]
      compileFork (fun, ar) = let f  = "fork_routine((act_t *) enter_" ++ fun ++ "( "
                                  lf = length f
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
      compVar (Left (r,_))      = "act->"  ++ r
      compVar (Right (Var _ n)) = "&act->" ++ n
      compVar (Right e)         = error $ "not a variable: " ++ show e

      -- | Compile either a reference or an expression to their correspending values.
      compVal :: Either Reference SSMExp -> String
      compVal (Left (r,_))      = "act->" ++ r ++ "->value"
      compVal (Right e)         = compLit e

      {- | Compile an argument to a function. We pass references as they are, but for literals
      we fetch them out of the sv-struct. -}
      compArg :: Either Reference SSMExp -> String
      compArg (Left (r,_))      = "act->" ++ r
      compArg (Right (Var _ v)) = "act->" ++ v ++ ".value"
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

      -- | Convert a type to the string representing the sv-type
      svtyp :: Type -> String
      svtyp t = if isReference t
          then concat ["sv_", prtyp (dereference t), "_t *"]
          else concat ["sv_", prtyp t, "_t "]
