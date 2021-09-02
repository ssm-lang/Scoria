{- | This module implements different strategies that are used to assign triggers to
sensitize/desensitize statements. The current strategy, `primitiveTriggerIDs`, work by
taking a naive approach and assigns a unique trigger to each reference that sensitizes
at least once.
-}
module SSM.Backend.C.Analysis.WaitAnalysis
  ( primitiveTriggerIDs
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
                                                , execState
                                                , get
                                                , gets
                                                , modify
                                                , put
                                                , runState
                                                )

import qualified Data.Map                      as Map

-- | State for the primitive trigger scheme
type PrimSt
  = ( Int
    , Map.Map Reference Int
    , Map.Map (Reference, Int) Int
    , Map.Map (Reference, Int) Int
    )

{- | Return a triple containing

  1. How many unique triggers are used
  2. A map that associates @(Reference, Int)@ -pairs with trigger IDs to use when
  sensitizing
  3. A map that associates @(Reference, Int)@ -pairs with trigger IDs to use when
  desensitizing.
-}
primitiveTriggerIDs
  :: [CStm] -> (Int, Map.Map (Reference, Int) Int, Map.Map (Reference, Int) Int)
primitiveTriggerIDs stmts =
  let st = (0, Map.empty, Map.empty, Map.empty)
      (ww, _, se, de) =
        execState (go stmts) (0, Map.empty, Map.empty, Map.empty)
  in  (ww, se, de)
 where
  go :: [CStm] -> State PrimSt ()
  go stmts = flip mapM_ stmts $ \x -> case x of
    Numbered n (Sensitize   r) -> recordSensitize r n
    Numbered n (Desensitize r) -> recordDesensitize r n
    Numbered n stm             -> return ()
    CWhile n c bdy             -> go bdy
    CIf n c thn els            -> go thn >> go els

  {- | Look up the unique trigger associated with a reference. If none exist yet,
    generate one and return that one. -}
  lookupTrigID :: Reference -> State PrimSt Int
  lookupTrigID r = do
    (i, m1, m2, m3) <- get
    case Map.lookup r m1 of
      Just id -> return id
      Nothing -> do
        put (i + 1, Map.insert r (i + 1) m1, m2, m3)
        return $ i + 1

  -- | Record the information that the reference @r@ on statement @n@ is sensitized
  recordSensitize :: Reference -> Int -> State PrimSt ()
  recordSensitize r n = do
    tid             <- lookupTrigID r
    (i, m1, m2, m3) <- get
    put (i, m1, Map.insert (r, n) tid m2, m3)

  -- | Record the information that the reference @r@ on statement @n@ is desensitized
  recordDesensitize :: Reference -> Int -> State PrimSt ()
  recordDesensitize r n = do
    tid             <- lookupTrigID r
    (i, m1, m2, m3) <- get
    put (i, m1, m2, Map.insert (r, n) tid m3)
