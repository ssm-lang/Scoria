{- | This module implements an analysis pass that annotates the body of a procedure with
information about which triggers to sensitize on.

Earlier in the project, there was a single statement @Wait :: [Reference] -> Stm@ and
no @Yield :: Stm@ statement. When generating code for @Wait@ you immediately knew how
many references you needed to sensitize (since you had them all in that list) and you
could record that you are now sensitizing @n@ references, to know how many unique
triggers you're going to need.

When the syntax was split up into the more fine-grained statements
`Sensitize`, `Desensitize` and
`Yield` in favour of @Wait@, this information become lost. When
generating code for a single `Sensitize` statement, it is not clear
how many references you are waiting on in total, or which of them in the sequence this
is. This makes it hard to know which trigger ID to sensitize on.

What this module does is that it implement an analysis pass that infers the now lost
information from the sequence of statements that make up a procedure body. It will
return the same procedure body as before, but where each statement has been given a
unique @Int@ ID. Alongside these new statements two maps are produced. They associate
a statement ID with a trigger ID. So if you generate code for @Numbered n (Sensitize r)@,
you will look up which ID @r@ should be @Sensitized@ on by checking which trigger ID @n@
is associated with in the sensitize map. Lastly, the size of the widest wait is inferred
from the procedure body and returned.

@
> analyseWait [ CreateRef r1 (Ref TInt)
              , SetRef r1 5
              , CreateRef r2 (Ref TBool)
              , SetRef r2 False
              , Sensitize r1
              , Sensitize r2
              , Yield
              , Desensitize r1
              , Desensitize r2
              , SetRef r2 True]
( [ Numbered 1 $ CreateRef r1 $ Ref TInt
  , Numbered 2 $ SetRef r2 5)
  , Numbered 3 $ CreateRef r2 $ Ref TBool
  , Numbered 4 $ SetRef r2 False
  , Numbered 5 $ Sensitize r1
  , Numbered 6 $ Sensitize r2
  , Numbered 7 $ Yield
  , Numbered 8 4 Desensitize r1
  , Numbered 9 $ Desensitize r2
  , Numbered 10 $ SetRef r2 True]
, [ (5, 1), (6, 2)]
, [ (8, 1), (9, 2)]
, 2
)
@

It's a bit crude, and should probably be made better later on when we wish to perform
different analysis passes. In particular, I am not too fond of the makeshift datatype
I implemented to number the statements.
-}
module SSM.Backend.C.Analysis.WaitAnalysis
    ( analyseWait
    ) where

import           SSM.Backend.C.Types            ( CStm(..) )
import           SSM.Core.Syntax                ( Procedure(body)
                                                , Reference
                                                , Stm
                                                    ( Desensitize
                                                    , If
                                                    , Sensitize
                                                    , While
                                                    )
                                                )

import           Control.Monad.State            ( State
                                                , gets
                                                , modify
                                                , runState
                                                )

import qualified Data.Map                      as Map

-- | Analysis state
data St = St
    { -- | ID of next statement
      nextLineNumber   :: Int
      -- | Size of the widest wait statement
    , widestwait       :: Int
      -- | Map associating sensitize statement IDs with trigger IDs
    , sensitizemap     :: Map.Map Int Int
      -- | Map associating desensitize statement IDs with trigger IDs
    , desensitizemap   :: Map.Map Int Int
      {- | Since references need to desensitize on the same reference they sensitized on,
      we must remember which triggers references were sensitized on. -}
    , currentSensitize :: Map.Map Reference Int
    }

{- | Analyse the body of a procedure and return a tuple with

1. The same procedure body, but with the statements given unique IDs  
2. Map associating sensitize statement IDs with trigger IDs to sensitize on  
3. Map associating desensitize statement IDs with trigger IDs to sensitize on  
4. The size of the widest wait statement, which is used to determine how many unique  
   triggers needs to be generated.
-}
analyseWait :: Procedure -> ([CStm], Map.Map Int Int, Map.Map Int Int, Int)
analyseWait p =
    let (body', st) = runState (numberStmts (body p))
            $ St 1 0 Map.empty Map.empty Map.empty
    in  (body', sensitizemap st, desensitizemap st, widestwait st)
  where
    {- | Assign unique numbers to statements and possible record trigger usage
    information. -}
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
