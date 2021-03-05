module CodeGen where

import Control.Monad.State
import Control.Monad.Writer
import GHC.Float
import Data.List

import Core

data CGenSt = CGenSt { widestWait  :: Int
                     , nextCase :: Int
                     , nameGen :: Int
                     }
type CGen a = StateT CGenSt -- widest wait encountered so far (determines the number of triggers) 
                (WriterT [String] 
                  IO) a

runCGen :: SSM () -> IO String
runCGen ssm = do
    let rw = evalStateT (genProcedure ssm) (CGenSt 0 0 0)
    out <- execWriterT rw
    return $ unlines out

-- f <- censor (const []) $ snd <$> listen (local (const 0) (compileSSM ssm))
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
          (NewRef n e k)    -> do indent $ "cv_int_t " ++ n ++ ";"
                                  specifics (k n)
          (SetRef r e k)    -> specifics (k ())
          (SetLocal e v k)  -> specifics (k ())
          (GetRef r k)      -> specifics (k (Lit (LBool True)))
          (If c thn (Just els) k) -> specifics (thn >> els >> k ())
          (If c thn Nothing k)  -> specifics (thn >> k ())
          (While c bdy k)   -> specifics (bdy >> k ())
          (After e r v k)   -> specifics (k ())
          (Changed r k)     -> specifics (k (Lit (LBool True))) -- dummy bool
          (Wait vars k)     -> do modify $ \st -> st { widestWait = max (widestWait st) (length vars)}
                                  specifics (k ())
          (Fork procs k)    -> specifics (k ())
          (Procedure n _ k) -> specifics (k ())
          (Argument n name (Left e) k)  -> do indent $ "cv_int_t " ++ name ++ ";"
                                              specifics (k ())
          (Argument n name (Right r) k) -> do indent $ "cv_int_t *" ++ name ++ ";"
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
    indent 0 "}"
  where
    getArgs :: SSM () -> [(String, String)]
    getArgs ssm = case ssm of
        (Procedure n _ k)             -> getArgs $ k ()
        (Argument n name (Left e) k)  -> ("cv_int_t ", name)   : getArgs (k ())
        (Argument n name (Right r) k) -> ("cv_int_t *", name) : getArgs (k ())
        _                             -> []
    
    initTrigger :: Int -> CGen ()
    initTrigger i = indent 4 $ "rar->trig" ++ show i ++ ".rar = (rar_t *) rar;"

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

      instant :: Int -> SSM () -> CGen ()
      instant level ssm = case ssm of
          (Return x)              -> return ()
          (NewRef n e k)          -> do
              indent level $ "initialize_int(&rar->" ++ n ++ ", " ++ compileLit e ++ ");"
              instant level $ k n
          (SetRef r e k)          -> do
              indent level $ "assign_int(&rar->" ++ r ++ ", rar->priority, " ++ compileLit e ++ ");"
              instant level $ k ()
          (SetLocal (Var e) v k)        -> do
              indent level $ "assign_int(&rar->" ++ e ++ ", rar->priority, " ++ compileLit v ++ ");"
              instant level $ k ()
          (GetRef r k)            -> do
              n <- fresh
              indent level $ "int " ++ n ++ " = *(rar->" ++ r ++ "->value);"
              instant level $ k (Var ("rar->" ++ n))
          (If c thn (Just els) k) -> do
              indent level $ "if( " ++ compileLit c ++ " ) {"
              instant (level + 4) thn
              indent level "} else {"
              instant (level + 4) els
              indent level "}"
              instant level $ k ()
          (If c thn Nothing k)    -> do
              indent level $ "if( " ++ compileLit c ++ " ) {"
              instant (level + 4) thn
              indent level "}"
              instant level $ k ()
          (While c bdy k)         -> do
              indent level $ "while ( " ++ compileLit c ++ " ) {"
              instant (level + 4) bdy
              indent level "}"
              instant level $ k ()
          (After e r v k)         -> do
              indent level $ "later_int(rar->" ++ r ++ ", now + " ++ compileLit e ++ ", " ++ compileLit v ++ ");"
              instant level $ k ()
          (Changed r k)           -> do
              b <- fresh
              indent level $ "bool " ++ b ++ " = event_on((cv_t *) rar->" ++ r ++ ");"
              instant level $ k (Var b)
          (Wait vars k)           -> do
              forM_ (zip vars [1..]) $ \(v,i) -> do
                  indent level $ "sensitize((cv_t*) rar->" ++ v ++ ", &rar->trig" ++ show i ++ ");"
                  pc' <- gets nextCase
                  indent level $ "rar->pc = " ++ show pc' ++ ";"
                  indent level   "return;"
                  newCase (level-4)
                  -- desensitize the triggers here
                  instant level $ k ()
          (Fork procs k)          -> do
              if length procs > 1
                then do
                    let new_depth_corr = integerLogBase 2 (toInteger (length procs))
                    indent level $ "uint8_t  new_depth    = rar->depth - " ++ show new_depth_corr ++ ";"
                    indent level   "uint32_t pinc         = 1 << new_depth;"
                    indent level   "uint32_t new_priority = rar->priority;"
                    let forks  = map (compileFork level "fork") procs
                    let forks' = intersperse ["", "new_priority = += pinc;"] forks
                    mapM_ (mapM_ (indent level)) forks'
                    pc' <- gets nextCase
                    indent level $ "rar->pc = " ++ show pc' ++ ";"
                else do
                    pc' <- gets nextCase
                    indent level $ "rar->pc = " ++ show pc' ++ ";"
                    mapM_ (indent level) $ compileFork level "call" (head procs)
              indent level "return;"
              newCase (level-4)
              instant level $ k ()

          (Procedure _ _ k)       -> instant level $ k ()
          (Argument _ _ _ k)      -> instant level $ k ()
          (Result _ _ k)          -> instant level $ k ()

      compileLit :: SSMExp -> String
      compileLit e = case e of
          Var s         -> "rar->" ++ s ++ "->value"
          Lit (LInt i)  -> show i
          Lit (LBool b) -> if b then "true" else "false"
          UOp e Neg     -> "(-" ++ compileLit e ++ ")"
          BOp e1 e2 op  -> "(" ++ compileLit e1 ++ ")" ++ compileOp op ++ "(" ++ compileLit e2 ++ ")"
        where
            compileOp :: BinOp -> String
            compileOp OPlus  = "+"
            compileOp OMinus = "-"
            compileOp OTimes = "*"
            compileOp OLT    = "<"
            compileOp OEQ    = "=="

      compileFork :: Int -> String -> SSM () -> [String]
      compileFork level method ssm@(Procedure n _ k) = 
          let constant_args = [ "(rar_t *) rar"
                              , "new_priority"
                              , "new_depth"
                              ]
              var_args      = getArgs $ k ()
              all_args      = constant_args ++ var_args
              prefix        = method ++ "((rar_t *) enter_" ++ n ++ "( "
              args          = map (\s -> replicate (length prefix - 1) ' ' ++ ", " ++ s) (tail all_args)
          in (prefix ++ head all_args) : args ++ ["));"]
        where
            getArgs :: SSM () -> [String]
            getArgs (Argument _ x (Left e) k)  = compileLit e : getArgs (k ())
            getArgs (Argument _ x (Right r) k) = r            : getArgs (k ())
            getArgs _                          = []

      fresh :: CGen String
      fresh = do
          c <- gets nameGen
          modify $ \st -> st { nameGen = nameGen st + 1}
          return $ "v" ++ show c