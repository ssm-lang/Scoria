module SSM.Backend.C.Analysis.WaitAnalysis where

import           SSM.Backend.C.Types
import           SSM.Core.Syntax

import           Control.Monad.State

import qualified Data.Map                      as Map

data St = St
    { nextLineNumber   :: Int
    , widestwait       :: Int
    , sensitizemap     :: Map.Map Int Int
    , desensitizemap   :: Map.Map Int Int
    , currentSensitize :: Map.Map Reference Int
    }

analyseWait :: Procedure -> ([CStm], Map.Map Int Int, Map.Map Int Int, Int)
analyseWait p =
    let (body', st) = runState (numberStmts (body p))
            $ St 1 0 Map.empty Map.empty Map.empty
    in  (body', sensitizemap st, desensitizemap st, widestwait st)
  where
    numberStmts :: [Stm] -> State St [CStm]
    numberStmts []             = return []
    numberStmts stmts@(x : xs) = case x of
        Sensitize _ -> do
          -- fetch all @Sensitize@ and number them
            let (sensitizes, rest) = getSensitizes stmts
            csensitizes <- mapM (\stm -> flip Numbered stm <$> nextNumber)
                                sensitizes

            -- record information about triggers and size of the wait block
            tagSensitizeWithTriggerId csensitizes
            widestWait $ length csensitizes

            -- number the rest of the statements and return them
            rest' <- numberStmts rest
            return $ csensitizes ++ rest'

        Desensitize r -> do
            n <- nextNumber
            tagDesensitizeWithTriggerId r n
            xs' <- numberStmts xs
            return $ Numbered n (Desensitize r) : xs'

        If c thn els -> do
            n    <- nextNumber
            thn' <- numberStmts thn
            els' <- numberStmts els
            xs'  <- numberStmts xs
            return $ CIf n c thn' els' : xs'

        While c bdy -> do
            n    <- nextNumber
            bdy' <- numberStmts bdy
            xs'  <- numberStmts xs
            return $ CWhile n c bdy' : xs'

        otherwise -> do
            n   <- nextNumber
            xs' <- numberStmts xs
            return $ Numbered n x : xs'

      where
        -- | Return the number of the next st
        nextNumber :: State St Int
        nextNumber = do
            n <- gets nextLineNumber
            modify $ \st -> st { nextLineNumber = n + 1 }
            return n

        {- | If @i@ in @widestWait i@ is larger than the previously largest widest wait,
        update the known widest wait to be @i@ instead. -}
        widestWait :: Int -> State St ()
        widestWait i =
            modify $ \st -> st { widestwait = max (widestwait st) i }

        {- | Input to this function is a list of sensitize statements. All the sensitize
        statements should make up the entire sequence of sensitize statements. This
        function will assign a trigger ID to each sensitize statement and record this
        information in the state. -}
        tagSensitizeWithTriggerId :: [CStm] -> State St ()
        tagSensitizeWithTriggerId stmts = mapM_ (uncurry uploadID)
            $ zip stmts [1 ..]
          where
            {- | @uploadID statementNumber triggerID@ registers that the statement
            numbered with @number@ should be sensitized with trigger id @triggerID@. -}
            uploadID :: CStm -> Int -> State St ()
            uploadID (Numbered l (Sensitize r)) tid = modify $ \st -> st
                { sensitizemap     = Map.insert l tid (sensitizemap st)
                , currentSensitize = Map.insert r tid (currentSensitize st)
                }
            uploadID _ _ =
                error "Robert made a mistake - should only be Numbered values"

        {- | Given a reference and the line number at which we wish to desensitize it,
        look up which trigger ID this reference was last sensitized on and register that
        this trigger ID should be desensitized, while also removing the state that says
        that the reference is currently sensitized on that trigger. -}
        tagDesensitizeWithTriggerId :: Reference -> Int -> State St ()
        tagDesensitizeWithTriggerId r l = do
            sensinfo <- gets currentSensitize
            let tid = case Map.lookup r sensinfo of
                    Just tid -> tid
                    Nothing  -> error $ concat
                        [ "error in wait analysis - trying to "
                        , "desensitize reference that was "
                        , "never sensitized"
                        ]
            modify $ \st -> st
                { desensitizemap   = Map.insert l tid (desensitizemap st)
                , currentSensitize = Map.delete r (currentSensitize st)
                }

        {- | Return a pair where the first element is all the `SSM.Core.Syntax.Sensitize`
        values and the second component is the rest of the statements. -}
        getSensitizes :: [Stm] -> ([Stm], [Stm])
        getSensitizes xs = (takeWhile isSensitize xs, dropWhile isSensitize xs)

    -- | Returns @True@ if a statement is a `SSM.Core.Syntax.Stm.Sensitize` value
    isSensitize :: Stm -> Bool
    isSensitize (Sensitize _) = True
    isSensitize _             = False
