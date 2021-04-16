{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module LowCodeGen where

import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy
import Control.Monad.Reader
import qualified Data.Map as Map
import Data.List
import LowCore

import System.IO.Unsafe

trace :: Show a => a -> a
trace x = unsafePerformIO $ putStrLn (show x) >> return x

-- | Different kind of field declarations we can do in the generated struct.
data FieldDec
  = -- | Declare a trigger with name "trig#".
    TriggerDec Int
  | -- | Declare a field with this type and name.
    FieldDec RTSType String
  | -- | Include the generic activation record fields.
    GenericFields
  deriving Show

-- | This datatype is used to render C code. The translation from [Stm] to [IR] will
-- remove any SSMExp and Reference and replace them with strings representing the C
-- code that means the same thing. E.g the reference "r0" would become "act->r0".
data IR
  = -- | `Function typ nam args bdy` == "typ nam(args) { bdy }" in c
    Function RTSType String [Param] [IR]
  | -- | Function prototype.
    Prototype RTSType String [Param]
  | -- | In the step function a generic `act_t*` argument is casted to a more specific
    -- `act_name_t*` type. This constructor represents that statement.
    -- Type to cast to and name of variable to cast.
    UpcastAct RTSType String
  | -- | Similarly there's a generic call to `enter` that's done in every enter function.
    -- Result type and name of procedure.
    UpcastEnter RTSType String
  | -- | `AssignStruct v1 v2` == "act->v1 = v2".
    AssignStruct String String
  | -- | Struct declaration. First argument is the name (renders as "act_name_t") and
    -- second field are the field declarations in the struct.
    Struct String [FieldDec]

  | -- | Switch statement that renders to "switch(act->pc) { ... }".
    Switch [IR]
  | -- | `IfFalse c l` == "if (!(c)) goto l;".
    IfFalse String Label
  | -- | `Label l` = "l:".
    Label Label
  | -- | `Goto l` == "goto l;".
    Goto Label
  | -- | `Case i bdy` == "case i: { bdy }".
    Case Int [IR]
  | -- | `IncPC i` = "act->pc = i".
    IncPC Int
  | -- | `Return` == `return;`
    Return
  | -- | Renders into a debug print statement "DEBUG_PRINT(x)"
    DebugPrint String

  | -- | Initialize a variable of the specific base type.
    Initialize BaseType String
  | -- | Initialize a trigger. Renders as "act->trig#.rar = (act_t *) act;"
    -- Only done in the enter function.
    InitializeTrigger Int
  | -- | Assign a value of the base type to a variable. The second field is the variable
    -- to receive the new value and the third field is the new value.
    Assign BaseType String String
  | -- | Schedule a delayed assignment. The second field is the delay, the third field
    -- is the variable to later receive the update and the fourth field is the new value.
    Later BaseType String String String
  | -- | In the variable represented by the first field, store the boolean which represents
    -- if the variable in the second field has been written to in this instant in time.
    EventOn String String
  | -- | Wait for any of these variables to be written to. The variables are paired up
    -- with an integer denoting which trigger in the struct that will be used.
    Sensitize [(String, Int)]
  | -- | Stop waiting for this trigger.
    Desensitize Int
  | -- | Fork a single call to this procedure with these arguments.
    Call (String, [String])
  | -- | Fork many procedures with these names and these arguments.
    ForkProcedures [(String, [String])]
  | -- | At the end of a step function we need to call leave to free up the struct memory.
    Leave String

  | -- | Render a blank line in the C file, for readability.
    Blank
  | -- | Verbatim code at a specified indentation level, rendered with a semicolon.
    Literal Int String
  | -- | Same as above but without a semicolon at the EOL.
    LiteralNoSemi Int String
  deriving (Show)

-- | Labels to use in conjunction with goto statements.
type Label = String

data BaseType
  = CInt
  | -- | from stdbool.h
    CBool

instance Show BaseType where
    show CInt  = "int"
    show CBool = "bool"

-- | Types recognized (not exhaustive) by the runtime system.
data RTSType
  = -- | Standard void type
    Void
  | -- | Act name renders as "act_name_t"
    Act String
  | -- | Signed integers, size not necessary
    SInt
  | -- | Unsigned integers, specify the size from [8, 16, 32, 64]
    UInt Int
  | -- | Booleans
    UBool
  | -- | Renders as trigger_t
    Trigger
  | -- | Types such as sv_int_t
    ScheduledVar BaseType
  | -- | Pointer to a type
    Pointer RTSType

instance Show RTSType where
    show Void              = "void"
    show (Act name)        = if null name
                               then "act_t"
                               else concat ["act_", name, "_t"]
    show SInt              = "int"
    show UBool             = "bool"
    show (UInt i)          = concat ["uint", show i, "_t"]
    show Trigger           = "trigger_t"
    show (ScheduledVar bt) = concat ["sv_", show bt, "_t"]
    show (Pointer t)       = concat [show t, "*"]

-- | A parameter to a function. The string is the name of the parameter and the
-- RTSType is the type of the parameter.
type Param = (String, RTSType)

-- | State maintained while compiling a program
data TRState = TRState
  { -- | Counter to generate fresh labels
    nextLabel :: Int,
    -- | Which number has the next case?
    ncase :: Int,
    -- | The size of the widest wait
    numwaits :: Int,
    -- | Local references declared with var
    localrefs :: [String]
  }

-- | Translation monad.
type TR a = State TRState a

-- | Run a TR computation.
runTR :: TR a -> a
runTR tra = evalState tra st
  where
      st :: TRState
      st = TRState 0 0 0 []

compile_ :: Bool -> Maybe Int -> Program -> String
compile_ False _ p = compile p
compile_ True c p  = let f = compile p
                         m = generateC $ mainIR p c
                     in unlines [f,m]

-- | This function takes a `Program` and returns a string, which contains the content
-- of the generated C file.
compile :: Program -> String
compile p = unlines code
  where
      -- | Entire C file.
      code :: [String]
      code = concat [header, generatedcode]

      -- | Generated code content.
      generatedcode :: [String]
      generatedcode = map generateC [ intersperse Blank structs
                                    , prototypes
                                    , intersperse Blank enters
                                    , intersperse Blank steps]

      -- | A sequence of `IR` statements representing all the structs, enter functions
      -- and step functions to appear in the C file.
      structs, enters, steps :: [IR]
      (structs, enters, steps) = unzip3 $ map compileProcedure $ Map.elems (funs p)

      -- | All function prototypes required in the C file.
      prototypes :: [IR]
      prototypes = concat $ flip map (Map.elems (funs p)) $ \f ->
          let (enter, step) = prototypeIR f
          in [enter, step, Blank]

      -- | Header to include in the generated C file.
      header :: [String]
      header = [ "#include \"peng-platform.h\""
               , "#include \"peng.h\""
               , "#include <stdio.h>"
               , ""
               , "#ifdef DEBUG"
               , "#define DEBUG_PRINT(x) printf(x);"
               , "#else"
               , "#define DEBUG_PRINT(x) while(0) {}"
               , "#endif"
               , ""
               ]

-- | This function takes a procedure and returns three `IR` statements that
-- represents the struct, enter function and step function of the procedure.
compileProcedure :: Procedure -> (IR, IR, IR)
compileProcedure p = (structTR, enterTR, stepTR)
  where
      (structTR,enterTR,stepTR) = runTR $ do
          step   <- stepIR p
          enter  <- enterIR p
          struct <- structIR p
          return (struct, enter, step)

-- | Returns the base type of a type. If the type is a reference it will unwrap the reference
-- to find the underlying basetype.
basetype_ :: Type -> BaseType
basetype_ TInt = CInt
basetype_ TBool = CBool
basetype_ (Ref t) = basetype_ t

-- | `paramtype t` returns the RTSType a parameter to a function, whose type in the
-- SSM EDSL is t, would have. E.g `Ref Int` would be `sv_int_t*`, `Exp Int` would be `int`.
paramtype :: Type -> RTSType
paramtype TInt    = SInt
paramtype TBool   = UBool
paramtype (Ref t) = Pointer $ ScheduledVar $ basetype_ t

-- | This class collects methods that helps us render expressions or
-- references appropriately depending on the situation we want to use it in.
class CType a where
  -- | The basetype of a value. A int* has the basetype int, and a normal int is already
  -- in the basetype form.
  basetype :: a -> BaseType

  -- | compArg returns the string that represents an `a` that we can pass to a call
  -- of `fork_routine`. E.g references are passed as sv_type_t* and expressions are
  -- passed as either ints or booleans.
  compArg :: a -> TR String

  -- | compVal returns the string that represents the underlying value, e.g and Int.
  compVal :: a -> TR String

  -- | compVar returns the string that symbolises a value of type sv_type_t*.
  -- This is used when we need a value of type sv_type_t* to pass to alter_type,
  -- assign_type, initialize_type etc.
  compVar :: a -> TR String

instance CType Reference where
  basetype (_, t) = basetype_ t

  compArg (n, _) = do
    refs <- gets localrefs
    let prefix = if n `elem` refs then "&" else ""
    return $ concat [prefix, "act->", n]

  compVal (n, _) = do
    refs <- gets localrefs
    let accessor = if n `elem` refs then "." else "->"
    return $ concat ["act->", n, accessor, "value"]

  compVar r = compArg r

instance CType SSMExp where
  basetype e = basetype_ $ expType e

  compArg (Var _ n) = return $ concat ["act->", n, ".value"]
  compArg e = compVal e

  compVal e = return $ compLit e

  compVar (Var _ n) = return $ concat ["&act->", n]
  compVar e = error $ concat ["compvar - not a variable: ", show e]

instance (CType a, CType b) => CType (Either a b) where
    basetype (Left a)  = basetype a
    basetype (Right b) = basetype b

    compArg (Left a)  = compArg a
    compArg (Right b) = compArg b

    compVal (Left a)  = compVal a
    compVal (Right b) = compVal b

    compVar (Left a)  = compVar a
    compVar (Right b) = compVar b

-- | Get the name of a variable expression. Throws an error if expression is not a variable.
getExpName :: SSMExp -> String
getExpName (Var _ n) = n
getExpName e         = error $ "getExpName - not a variable: " ++ show e

-- | Compile an expression into the string that represent its semantic value.
-- By semantic I mean that the result will be of type `int` or `bool`:
compLit :: SSMExp -> String
compLit e = case e of
  Var _ e -> "act->" ++ e ++ ".value"
  
  Lit _ l -> case l of
    LInt i      -> if i >= 0 then show i else "(" ++ show i ++ ")"
    LBool True  -> "true"
    LBool False -> "false"
  
  UOp _ e op -> case op of
    Neg -> "(-" ++ compLit e ++ ")"
  
  BOp _ e1 e2 op -> case op of
    OPlus  -> "(" ++ compLit e1 ++ " + " ++ compLit e2 ++ ")"
    OMinus -> "(" ++ compLit e1 ++ " - " ++ compLit e2 ++ ")"
    OTimes -> "(" ++ compLit e1 ++ " * " ++ compLit e2 ++ ")"
    OLT    -> "(" ++ compLit e1 ++ " < " ++ compLit e2 ++ ")"
    OEQ    -> "(" ++ compLit e1 ++ " == " ++ compLit e2 ++ ")"

-- | Generate a fresh label.
freshLabel :: TR Label
freshLabel = do
    i <- gets nextLabel
    modify $ \st -> st { nextLabel = i + 1 }
    return $ "L" ++ show i

-- | Infix operator for applying a unlifted argument to a lifted function.
-- Surprised that I did not find this anywhere.
(<#>) :: Applicative f => f (a -> b) -> a -> f b
fa <#> b = fa <*> pure b
infixl 4 <#>

-- | This function generates two `IR` statements that represents the two function
-- prototypes for the enter and step function.
prototypeIR :: Procedure -> (IR, IR)
prototypeIR p = ( Prototype (Pointer $ Act (name p)) (concat ["enter_", name p]) args
                , Prototype Void (concat ["step_", name p]) [("gen_act", Pointer $ Act "")]
                )
  where
      -- | All procedure arguments.
      args :: [Param]
      args = genargs ++ dynargs (arguments p)

      -- | Procedure arguments that all procedures need.
      genargs :: [Param]
      genargs = [ ("caller"  , Pointer $ Act "")
                , ("priority", UInt 32)
                , ("depth"   , UInt 8)
                ]
    
      -- | The procedure specific arguments.
      dynargs :: [(String, Type)] -> [Param]
      dynargs xs = flip map xs $ \(n,t) -> (n, paramtype t)

-- | This function generates a `IR` statement that represents the struct declaration
-- of this procedure.
structIR :: Procedure -> TR IR
structIR p = do
    let paramfields = map paramfield $ arguments p
    let dynfields' = dynfields $ body p
    triggerfields <- triggerdecs
    return $ Struct (name p) $ concat [ [GenericFields]
                                  , paramfields
                                  , dynfields'
                                  , triggerfields
                                  ]
  where
      -- | The field declarations that arise from the procedure arguments.
      paramfield :: (String, Type) -> FieldDec
      paramfield (n,t) = FieldDec t' n
        where
            t' = if isReference t
                then Pointer $ ScheduledVar $ basetype_ t
                else ScheduledVar $ basetype_ t

      -- | The field declarations that arise from us creating variables in a
      -- procedure body, e.g from NewRef and GetRef.
      dynfields :: [Stm] -> [FieldDec]
      dynfields xs = concat $ flip map xs $ \x -> case x of
          NewRef n e _  -> [ FieldDec (ScheduledVar (basetype e)) (getVarName n) ]
          GetRef n e _  -> [ FieldDec (ScheduledVar (basetype e)) (getVarName n) ]
          Changed n e _ -> [ FieldDec (ScheduledVar (basetype e)) (getVarName n) ]
          _             -> []
    
      -- | Generate trigger declarations. The only variable thing in a trigger
      -- declaration is the number in its variable name.
      triggerdecs :: TR [FieldDec]
      triggerdecs = do
          i <- gets numwaits
          return $ flip map [1..i] $ \j -> TriggerDec j

-- | This function generates an `IR` statement that represents the enter function
-- of this procedure.
enterIR :: Procedure -> TR IR
enterIR p = do
    let fun    = Function (Pointer $ Act $ name p) (concat ["enter_", name p]) args
    let cast   = UpcastEnter (Pointer $ Act $ name p) $ name p
    let assvar = concat $ map assignParam $ arguments p
    setuptrigs <- setupTriggers
    return $ fun $ concat [ [cast]
                          , assvar
                          , setuptrigs
                          ]
    
  where
      -- | All arguments to this enter function.
      args :: [Param]
      args = genargs ++ dynargs (arguments p)

      -- | Generic arguments that goes into all enter function.
      genargs :: [Param]
      genargs = [ ("caller"  , Pointer $ Act "")
                , ("priority", UInt 32)
                , ("depth"   , UInt 8)
                ]
    
      -- | Procedure dynamic arguments to enter function.
      dynargs :: [(String, Type)] -> [Param]
      dynargs xs = flip map xs $ \(n,t) -> (n, paramtype t)

      -- | This function generates `IR` statements that initializes the procedure
      -- arguments in the enter function. This is done by assigning them to the struct
      -- fields after (maybe) having initialized the struct variable.
      assignParam :: (String, Type) -> [IR]
      assignParam (n,Ref t) = [AssignStruct n n]
      assignParam (n,t)     = [ Initialize (basetype_ t) (concat ["&act->", n])
                              , Assign (basetype_ t) (concat ["&act->", n]) n
                              ]

      -- | This function will generate a sequence of `IR` instructions that represents
      -- trigger initialization calls meant to appear in the enter function.
      setupTriggers :: TR [IR]
      setupTriggers = do
          i <- gets numwaits
          return $ map InitializeTrigger [1..i]

-- | This function takes a procedure and generates an `IR` instruction that represents
-- the step function of this procedure.
stepIR :: Procedure -> TR IR
stepIR p = do
    let fun = Function Void (concat ["step_", name p]) [("gen_act", Pointer $ Act "")]
    let cast = UpcastAct (Pointer $ Act $ name p) "gen_act"
    cases <- stmts $ body p
    let leave = Leave $ name p
    return $ fun [ cast
                 , Switch cases
                 , leave
                 ]
  where
      -- | Takes as input a sequence of `Stm` and returns a sequence of `IR` statements.
      -- The `IR` statements in the sequence will all be case blocks meant to go inside
      -- of a switch statement.
      stmts :: [Stm] -> TR [IR]
      stmts [] = return []
      stmts xs = do
          let (block, rest) = untilBlock xs
          caseblock <- gencase block
          block' <- nextcase <*> endcase caseblock
          (:) block' <$> stmts rest

      -- | This function will return a function that takes a sequence of `IR` statements
      -- should be in a case block and returns a case block with the next case number
      -- and those `IR` statements in its body.
      nextcase :: TR ([IR] -> IR)
      nextcase = do
          i <- gets ncase
          modify $ \st -> st { ncase = i + 1 }
          return $ Case i
    
      -- | Return a `IR` statement that increments the program counter to the next case.
      incpc :: TR IR
      incpc = do
          i <- gets ncase
          return $ IncPC i

      -- | This function will take a sequence of `IR` statements and insert a pc increment and
      -- return statement at the end. If the last `IR` statement is a `Call` statement the
      -- program counter will be incremented before the `Call` instruction, as the `call`
      -- method in C will immediately invoke the procedure rather than just scheduling it.
      endcase :: [IR] -> TR [IR]
      endcase xs = do
          pc <- incpc
          case last xs of
            Call _ -> return $ concat [init xs, [pc, last xs, Return]]
            _      -> return $ concat [xs, [pc, Return]]

      -- | This function will generate a sequence of `IR` statements for each `Stm`.
      gencase :: [Stm] -> TR [IR]
      gencase xs = fmap concat $
        flip mapM xs $ \x -> case x of
            NewRef n e v     -> do
                modify $ \st -> st { localrefs = getExpName e : localrefs st }
                init <- Initialize (basetype e) <$> compVar e
                assi <- Assign (basetype e) <$> compVar e <*> compVal v
                return $ [init, assi]
            GetRef n v r   -> sequence [Assign (basetype v)  <$> compVar v <*> compVal r]
            SetRef r e     -> sequence [Assign (basetype r)  <$> compVar r <*> compVal e]
            SetLocal e1 e2 -> sequence [Assign (basetype e1) <$> compVar e1 <*> compVal e2]

            If c thn els -> do
                l1 <- freshLabel
                l2 <- freshLabel

                iffalse <- IfFalse <$> compVal c <#> l1
                thn' <- gencase thn
                els' <- gencase els

                return $ concat [ [iffalse]
                                , thn'
                                , [Goto l2
                                , Blank
                                , Label l1]
                                , els'
                                , [Blank
                                , Label l2]
                                ]
            While c bdy  -> do
                l1 <- freshLabel
                l2 <- freshLabel

                iffalse <- IfFalse <$> compVal c <#> l2
                bdy' <- gencase bdy
                return $ concat [ [Label l1
                                , iffalse]
                                , bdy'
                                , [Goto l1
                                , Label l2]
                                ]
            Skip         -> return []

            After d r v   -> sequence [Later (basetype r) <$> compVal d <*> compVar r <*> compVal v]
            Changed n v r -> sequence [EventOn <$> compVar v <*> compVar r]

            Wait refs  -> do
                modify $ \st -> st { numwaits = max (numwaits st) (length refs)}
                sensitizes <- forM (zip refs [1..]) $ \(r,i) -> do
                    var <- compVar r
                    return (var,i)
                return [Sensitize sensitizes]
            Fork calls -> do
                let debugstr = unwords $ ["fork"] ++ (fst . unzip) calls
                callstrings <- forM calls $ \(n,args) -> do
                    args' <- mapM compArg args
                    return (n, args')
                return $ if length callstrings == 1
                        then [DebugPrint debugstr, Call (head callstrings)]
                        else [DebugPrint debugstr, ForkProcedures callstrings]
          

-- | Returns a tuple where the first cell list contains the statements up to and including
-- the next blocking statement, and the second cell contains the statements left after
-- the blocking statement.
untilBlock :: [Stm] -> ([Stm], [Stm])
untilBlock [] = ([], [])
untilBlock (x : xs) = case x of
  Wait _ -> ([x], xs)
  Fork _ -> ([x], xs)
  _      -> ([x],[]) <> untilBlock xs

mainIR :: Program -> Maybe Int -> [IR]
mainIR p c = [ Function Void "top_return" [("act", Pointer $ Act "")] [Return]
             , Blank
             , Function Void "main" [] bdy
             ]
  where
      bdy :: [IR]
      bdy = concat $ [ [Literal 4 $ "act_t top = { .step = top_return }"]
                     , refinits
                     , [Literal 4 $ concat $ ["fork_routine((act_t *) enter_", main p
                                             ,"(", intercalate ", " entryargs, "))"]
                     , Literal 4 "tick()"
                     , debugprint 4
                     , LiteralNoSemi 4 $ maybe "" (\i -> "int counter = " ++ show i ++ ";") c
                     , LiteralNoSemi 4 $ maybe "while(1) {" (const "while(counter) {") c
                     , Literal 8 "now = next_event_time()"
                     , LiteralNoSemi 8 "if(now == NO_EVENT_SCHEDULED)"
                     , Literal 12 "break"
                     , Literal 8 "tick()"
                     , debugprint 8
                     , LiteralNoSemi 8 $ maybe "" (const "counter = counter - 1;") c
                     , LiteralNoSemi 4 "}"]
                     , printrefs
                     ]

      -- | The sequence of `IR` statements that represent the initialization of
      -- the program input references.
      refinits :: [IR]
      refinits = concat $ mapRight (args p) $ \(r,t) ->
          [ Literal 4 $ concat $ ["sv_", show (basetype_ t), "_t ", r]
          , Literal 4 $ concat $ ["initialize_", show (basetype_ t), "(&", r, ")"]
          , Literal 4 $ concat $ [r, ".value = ", defaultValue t]]

      -- | Default value for references of different types.
      defaultValue :: Type -> String
      defaultValue TInt    = "0"
      defaultValue TBool   = "false"
      defaultValue (Ref t) = defaultValue t

      -- | Map over a list of eithers and only keep the results obtained by mapping
      -- a function over the `Right` elements.
      mapRight :: [Either a b] -> (b -> c) -> [c]
      mapRight [] _           = []
      mapRight (Right a:xs) f = f a : mapRight xs f
      mapRight (_:xs) f       = mapRight xs f

      entryargs :: [String]
      entryargs = [ "&top"
                  , "PRIORITY_AT_ROOT"
                  , "DEPTH_AT_ROOT"
                  ] ++ map (either compLit ((++) "&" . fst)) (args p)

      printrefs :: [IR]
      printrefs = mapRight (args p) $ \(r,t) ->
          Literal 4 $ concat ["printf(\"", r, " %u", "\\n\", ", r, ".value)"]

      debugprint :: Int -> IR
      debugprint i = Literal i $ concat ["printf(\"now %lu eventqueuesize %d\\n\", now, event_queue_len)"]

{- ***************** Code generation *************** -}

-- | Monad for generating C code. The read only integer represents the indentation level at
-- which to output code, and the writer output is the actual code that was generated.
type CGen a = ReaderT Int (Writer [String]) a

-- | Run the code generating function `irToC` and return the output emitted.
generateC :: [IR] -> String
generateC xs = unlines $ execWriter $ runReaderT (irToC xs) $ 0

-- | Generate output in the form of C code from the input IR instructions.
irToC :: [IR] -> CGen ()
irToC ir = flip mapM_ ir $ \x -> case x of
    Function typ name params bdy -> do
        let args = intercalate ", " $ flip map params $ \(n,t) -> concat [show t, " ", n]
        emit $ concat [show typ, " ", name, "(", args, ") {"]
        indent $ irToC bdy
        emit "}"
    Prototype typ name params -> do
        let args = intercalate ", " $ flip map params $ \(n,t) -> concat [show t, " ", n]
        emit $ concat [show typ, " ", name, "(", args, ");"]
    UpcastAct typ name         ->
        emit $ concat [show typ, " act = (", show typ, ") ", name, ";"]
    UpcastEnter typ name       ->
        emit $ concat [ show typ
                      , " act = ("
                      , show typ
                      , ") enter(sizeof(act_", name, "_t), step_"
                      , name
                      , ", caller, priority, depth);"]
    AssignStruct var val       ->
        emit $ concat ["act->", var, " = ", val, ";"]
    Struct typedef fields      -> do
        emit "typedef struct {"
        indent $ forM_ fields $ \f -> case f of
            TriggerDec i     -> emit $ concat ["trigger_t trig", show i, ";"]
            FieldDec typ var -> emit $ concat [show typ, " ", var, ";"]
            GenericFields    -> emit "ACTIVATION_RECORD_FIELDS;"
        emit $ concat ["} act_", typedef, "_t;"]

    Switch stmts    -> do
        emit "switch(act->pc) {"
        indent $ irToC stmts
        emit "}"
    IfFalse tst lbl ->
        emit $ concat ["if (!(", tst, ")) goto ", lbl, ";"]
    Label lbl       -> emit $ concat [lbl, ":"]
    Goto lbl        -> emit $ concat ["goto ", lbl, ";"]
    Case i bdy      -> do
        emit $ concat ["case ", show i, ": {"]
        indent $ irToC bdy
        emit "}"
    IncPC i         -> emit $ concat ["act->pc = ", show i, ";"]
    Return          -> emit "return;"
    DebugPrint str  -> emit $ concat ["DEBUG_PRINT(\"", str, "\")"]

    Initialize bt var   ->
        emit $ concat ["initialize_", show bt, "(", var, ");"]
    InitializeTrigger i ->
        emit $ concat ["act->trig", show i, ".act = (act_t *) act;"]
    Assign bt var val   ->
        emit $ concat ["assign_", show bt, "(", var, ", act->priority, ", val, ");"]
    Later bt t var val  ->
        emit $ concat ["later_", show bt, "(", var, ", now + ", t, ", ", val, ");"]
    EventOn res ref     -> do
        let eventon = concat ["event_on((sv_t *) ", ref, ");"]
        irToC [Assign (basetype_ TBool) res eventon]
    Sensitize waits     -> 
        forM_ waits $ \(v,i) -> do
            emit $ concat ["sensitize((sv_t *)", v, ", &act->trig", show i, ");"]
    Desensitize i       ->
        emit $ concat ["desensitize(&act->trig", show i, ");"]
    Call (fun,args)     ->
        emit $ concat ["call((act_t *) ", enter_ fun "act->priority" "act_depth" args, ");"]
    ForkProcedures funs -> do
        emit "{"
        indent $ do
            let new_depth = ceiling (logBase 2 (fromIntegral (length funs)))
            emit $ concat ["uint8_t new_depth = act->depth - ", show new_depth,";"]
            emit "uint32_t pinc = 1 << new_depth;"
            emit "uint32_t new_priority = act->priority;"
            intercalateM (emit "new_priority += pinc;") $ flip map funs $ \(fun,args) -> do
                emit $ concat ["fork_routine((act_t *) "
                              , enter_ fun "new_priority" "new_depth" args, ");"]
        emit "}"
    Leave name          ->
        emit $ concat ["leave((act_t *) act, sizeof(act_", name, "_t));"]

    Blank                 -> emit ""
    Literal ind str       -> tell [concat [replicate ind ' ', str, ";"]]
    LiteralNoSemi ind str -> tell [concat [replicate ind ' ', str]]
  where
      -- | Emit a string output at the indentation level indicated by the reader environment.
      emit :: String -> CGen ()
      emit str = do
          i <- ask
          tell [replicate i ' ' ++ str]

      -- | Run a code generating computation with increased indentation, making all output
      -- generated by `emit` appear one indentation level deeper.
      indent :: CGen a -> CGen a
      indent = local (+4)

      -- | Given the name of the procedure to enter, the string representing the priority
      -- argument, a string representing the depth argument and the actual procedure
      -- arguments themselves, generates the enter_name function call.
      enter_ :: String -> String -> String -> [String] -> String
      enter_ name prio depth args =
          let args' = intercalate ", " $ ["(act_t *) act", prio, depth] ++ args
          in  concat $ ["enter_", name, "(", args', ")"]
    
      -- | This function will intercalate a monadic computation between a list of
      -- monadic computations. intercalate ma [m1,m2,m3] == m1 >> ma >> m2 >> ma >> m3.
      intercalateM :: Monad m => m a -> [m a] -> m [a]
      intercalateM _ [] = return []
      intercalateM _ [x] = x >>= (return . flip (:) [])
      intercalateM ma (x:y:xs) = do
          x' <- x
          y' <- ma
          xs' <- intercalateM ma (y:xs)
          return $ x' : y' : xs'