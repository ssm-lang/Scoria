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
module SSM.Backend.C.Analysis.WaitAnalysis where

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
                                                , get
                                                , put
                                                , modify
                                                , runState
                                                , execState
                                                )

import qualified Data.Map                      as Map

-- | Keys of the sensitize/desensitize map are pairs of references and statement numbers
type TriggerKey = (Reference, Int)
-- | A map associating `TriggerKey` with a trigger ID
type TriggerMap = Map.Map TriggerKey Int

-- | State for the primitive trigger scheme
type PrimSt = (Int, Map.Map Reference Int, TriggerMap, TriggerMap)

-- | Return (how many unique triggers are needed, sensitize map, desensitize map)
primitiveTriggerIDs :: [CStm] -> (Int, TriggerMap, TriggerMap)
primitiveTriggerIDs stmts =
    let st = (0, Map.empty, Map.empty, Map.empty)
        (ww,_,se, de) = execState (go stmts) (0, Map.empty, Map.empty, Map.empty)
    in (ww, se, de)
  where
    go :: [CStm] -> State PrimSt ()
    go stmts = flip mapM_ stmts $ \x -> case x of
      Numbered n (Sensitize r)    -> recordSensitize r n
      Numbered n (Desensitize r)  -> recordDesensitize r n
      Numbered n stm  -> return ()
      CWhile n c bdy  -> go bdy
      CIf n c thn els -> go thn >> go els

    {- | Look up the unique trigger associated with a reference. If none exist yet,
    generate one and return that one. -}
    lookupTrigID :: Reference -> State PrimSt Int
    lookupTrigID r = do
      (i,m1,m2,m3) <- get
      case Map.lookup r m1 of
        Just id -> return id
        Nothing -> do
          put (i+1, Map.insert r (i + 1) m1, m2, m3)
          return $ i + 1

    -- | Record the information that the reference @r@ on statement @n@ is sensitized
    recordSensitize :: Reference -> Int -> State PrimSt ()
    recordSensitize r n = do
      tid <- lookupTrigID r
      (i,m1,m2,m3) <- get
      put (i, m1, Map.insert (r,n) tid m2, m3)

    -- | Record the information that the reference @r@ on statement @n@ is desensitized
    recordDesensitize :: Reference -> Int -> State PrimSt ()
    recordDesensitize r n = do
      tid <- lookupTrigID r
      (i,m1,m2,m3) <- get
      put (i, m1, m2, Map.insert (r,n) tid m3)
