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


data FieldDec
  = TriggerDec Int
  | FieldDec RTSType String
  | GenericFields
  deriving Show

data IR
  = Function RTSType String [Param] [IR]
  | Prototype RTSType String [Param]
  | UpcastAct RTSType String
  | UpcastEnter RTSType String
  | CastGeneric RTSType String
  | AssignStruct String String
  | Struct String [FieldDec]

  | Switch [IR]
  | IfFalse String Label
  | Label Label
  | Goto Label
  | Case Int [IR]
  | IncPC Int
  | Return

  | -- Methods from the SSM runtime
    Initialize BaseType String
  | InitializeTrigger Int
  | Assign BaseType String String
  | Later BaseType String String String
  | EventOn String String
  | Sensitize [(String, Int)] -- [(reference, triggernumber)]
  | Desensitize Int
  | Call (String, [String])
  | ForkProcedures [(String, [String])]
  | Leave String

  | Blank -- Blank linkes for readability
  | Literal Int String -- Verbatim code
  | LiteralNoSemi Int String
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
--     RTSType is the type of the parameter.
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

-- | Translation monad
type TR a = State TRState a

runTR :: TR a -> a
runTR tra = evalState tra st
  where
      st :: TRState
      st = TRState 0 0 0 []

compile :: Program -> String
compile p = unlines code
  where
      code :: [String]
      code = concat [header, generatedcode]

      generatedcode :: [String]
      generatedcode = map generateC [ intersperse Blank structs
                                    , prototypes
                                    , intersperse Blank enters
                                    , intersperse Blank steps]

      structs, enters, steps :: [IR]
      (structs, enters, steps) = unzip3 $ map compileProcedure $ Map.elems (funs p)

      prototypes :: [IR]
      prototypes = concat $ flip map (Map.elems (funs p)) $ \f ->
          let (enter, step) = prototypeIR f
          in [enter, step, Blank]

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

compileProcedure :: Procedure -> (IR, IR, IR)
compileProcedure p = (structTR, enterTR, stepTR)
  where
      (structTR,enterTR,stepTR) = runTR $ do
          step   <- stepIR (name p) (body p)
          enter  <- enterIR (name p) (arguments p) (body p)
          struct <- structIR (name p) (arguments p) (body p)
          return (struct, enter, step)

-- | Returns the base type of a type. If the type is a reference it will unwrap the reference
--     to find the underlying basetype.
basetype_ :: Type -> BaseType
basetype_ TInt = CInt
basetype_ TBool = CBool
basetype_ (Ref t) = basetype_ t

{- | `paramtype t` returns the RTSType a parameter to a function, whose type in the
     SSM EDSL is t, would have. E.g `Ref Int` would be `sv_int_t*`, `Exp Int` would be `int`. -}
paramtype :: Type -> RTSType
paramtype TInt    = SInt
paramtype TBool   = UBool
paramtype (Ref t) = Pointer $ ScheduledVar $ basetype_ t

-- | This class collects methods that helps us render expressions or
--     references appropriately depending on the situation we want to use it in.
class CType a where
  -- | The basetype of a value. A int* has the basetype int, and a normal int is already
  --   in the basetype form.
  basetype :: a -> BaseType

  -- | compArg returns the string that we can apply a method to in order to pass the
  --         `a` as an argument. E.g a SSMExp becomes a `int` or a `bool`, while a reference will
  --         become a value of type `sv_type_t*`
  compArg :: a -> TR String

  -- | compVal returns the string that represents the underlying value, e.g and Int.
  compVal :: a -> TR String

  -- | compVar returns the string that symbolises a value of type sv_type_t*.
  --         It is used when we need to e.g apply a function to a reference to a
  --         variable.
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

getExpName :: SSMExp -> String
getExpName (Var _ n) = n
getExpName e         = error $ "getExpName - not a variable: " ++ show e

-- | Compile an expression into the string that represent its semantic value.
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

freshLabel :: TR Label
freshLabel = do
    i <- gets nextLabel
    modify $ \st -> st { nextLabel = i + 1 }
    return $ "L" ++ show i

infixl 4 <#>
(<#>) :: Applicative f => f (a -> b) -> a -> f b
fa <#> b = fa <*> pure b

prototypeIR :: Procedure -> (IR, IR)
prototypeIR p = ( Prototype (Pointer $ Act (name p)) (concat ["enter_", name p]) args
                , Prototype Void (concat ["step_", name p]) [("gen_act", Pointer $ Act "")]
                )
  where
      args :: [Param]
      args = genargs ++ dynargs (arguments p)

      genargs :: [Param]
      genargs = [ ("caller"  , Pointer $ Act "")
                , ("priority", UInt 32)
                , ("depth"   , UInt 8)
                ]
    
      dynargs :: [(String, Type)] -> [Param]
      dynargs xs = flip map xs $ \(n,t) -> (n, paramtype t)

structIR :: String -> [(String, Type)] -> [Stm] -> TR IR
structIR name params xs = do
    let paramfields = map paramfield params
    let dynfields' = dynfields xs
    triggerfields <- triggerdecs
    return $ Struct name $ concat [ [GenericFields]
                                  , paramfields
                                  , dynfields'
                                  , triggerfields
                                  ]
  where
      paramfield :: (String, Type) -> FieldDec
      paramfield (n,t) = FieldDec t' n
        where
            t' = if isReference t
                then Pointer $ ScheduledVar $ basetype_ t
                else ScheduledVar $ basetype_ t

      dynfields :: [Stm] -> [FieldDec]
      dynfields xs = concat $ flip map xs $ \x -> case x of
          NewRef n e _  -> [ FieldDec (ScheduledVar (basetype e)) (getVarName n) ]
          GetRef n e _  -> [ FieldDec (ScheduledVar (basetype e)) (getVarName n) ]
          Changed n e _ -> [ FieldDec (ScheduledVar (basetype e)) (getVarName n) ]
          _             -> []
    
      triggerdecs :: TR [FieldDec]
      triggerdecs = do
          i <- gets numwaits
          return $ flip map [1..i] $ \j -> TriggerDec j

enterIR :: String -> [(String, Type)] -> [Stm] -> TR IR
enterIR name params xs = do
    let fun    = Function (Pointer $ Act name) ("enter_" ++ name) args
    let cast   = UpcastEnter (Pointer $ Act name) name
    let assvar = concat $ map assignParam params
    setuptrigs <- setupTriggers
    return $ fun $ concat [ [cast]
                          , assvar
                          , setuptrigs
                          ]
    
  where
      args :: [Param]
      args = genargs ++ dynargs params

      genargs :: [Param]
      genargs = [ ("caller"  , Pointer $ Act "")
                , ("priority", UInt 32)
                , ("depth"   , UInt 8)
                ]
    
      dynargs :: [(String, Type)] -> [Param]
      dynargs xs = flip map xs $ \(n,t) -> (n, paramtype t)

      assignParam :: (String, Type) -> [IR]
      assignParam (n,Ref t) = [AssignStruct n n]
      assignParam (n,t)     = [ Initialize (basetype_ t) (concat ["&act->", n])
                              , Assign (basetype_ t) (concat ["&act->", n]) n
                              ]

      setupTriggers :: TR [IR]
      setupTriggers = do
          i <- gets numwaits
          return $ map InitializeTrigger [1..i]


stepIR :: String -> [Stm] -> TR IR
stepIR name xs = do
    let fun = Function Void ("step_" ++ name) [("gen_act", Pointer $ Act "")]
    let cast = UpcastAct (Pointer $ Act name) "gen_act"
    cases <- body xs
    let leave = Leave name
    return $ fun [ cast
                 , Switch cases
                 , leave
                 ]
  where
      body :: [Stm] -> TR [IR]
      body [] = return []
      body xs = do
          let (block, rest) = untilBlock xs
          stmts <- gencase block
          block' <- nextcase <*> endcase stmts
--          block <- nextcase <*> ((\b -> b ++ [pc, Return]) <$> gencase block)
          (:) block' <$> body rest

      nextcase :: TR ([IR] -> IR)
      nextcase = do
          i <- gets ncase
          modify $ \st -> st { ncase = i + 1 }
          return $ Case i
    
      incpc :: TR IR
      incpc = do
          i <- gets ncase
          return $ IncPC i

      endcase :: [IR] -> TR [IR]
      endcase xs = do
          pc <- incpc
          case last xs of
            Call _ -> return $ concat [init xs, [pc, last xs, Return]]
            _      -> return $ concat [xs, [pc, Return]]

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
                callstrings <- forM calls $ \(n,args) -> do
                    args' <- mapM compArg args
                    return (n, args')
                return $ if length callstrings == 1
                        then [Call (head callstrings)]
                        else [ForkProcedures callstrings]
          

-- | Returns a tuple where the first cell list contains the statements up to and including
--    the next blocking statement, and the second cell contains the statements left after
--    the blocking statement.
untilBlock :: [Stm] -> ([Stm], [Stm])
untilBlock [] = ([], [])
untilBlock (x : xs) = case x of
  Wait _ -> ([x], xs)
  Fork _ -> ([x], xs)
  _ ->
    let (xs', ys') = untilBlock xs
     in (x : xs', ys')

{- ***************** Code generation *************** -}

type PP a = ReaderT Int (Writer [String]) a

generateC :: [IR] -> String
generateC xs = unlines $ execWriter $ runReaderT (irToC xs) $ 0

irToC :: [IR] -> PP ()
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
    CastGeneric typ name       -> undefined
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
      emit :: String -> PP ()
      emit str = do
          i <- ask
          tell [replicate i ' ' ++ str]

      indent :: PP a -> PP a
      indent = local (+4)

      enter_ :: String -> String -> String -> [String] -> String
      enter_ name prio depth args =
          concat [ "enter_", name
                 , "((act_t *) act, "
                 , prio, ", "
                 , depth, ", "
                 , intercalate ", " args
                 , ")"
                 ]
    
      intercalateM :: Monad m => m a -> [m a] -> m [a]
      intercalateM _ [] = return []
      intercalateM _ [x] = x >>= (return . flip (:) [])
      intercalateM ma (x:y:xs) = do
          x' <- x
          y' <- ma
          xs' <- intercalateM ma (y:xs)
          return $ x' : y' : xs'