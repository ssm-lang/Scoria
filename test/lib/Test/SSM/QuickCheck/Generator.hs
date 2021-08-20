{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Test.SSM.QuickCheck.Generator where

import Data.List
import Data.Word
import Data.Int
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set

import SSM.Core.Syntax
import SSM.Util.HughesList hiding ( (++) )

import Test.SSM.QuickCheck.Shrink hiding (Ref, Variable)

import Test.QuickCheck
import Test.QuickCheck.Gen.Unsafe ()
import Control.Monad.Reader
import Control.Monad.State

import System.IO.Unsafe

trace :: Show a => a -> a
trace x = unsafePerformIO $ putStrLn (show x) >> return x

for :: [a] -> (a -> b) -> [b]
for = flip map

basetypes :: [Type]
basetypes = [TUInt8, TUInt64, TInt32, TInt64, TBool, TEvent]

instance Arbitrary Type where
  arbitrary = elements $ basetypes ++ map Ref basetypes

instance Arbitrary SSMTimeUnit where
  arbitrary = elements [ SSMNanosecond
                       , SSMMicrosecond
                       , SSMMillisecond
                       , SSMSecond
                       , SSMMinute
                       , SSMHour
                       ]

type Procedures = [(Ident, [(Ident, Type)], [Stm])]
type Variable   = (Ident, Type)

genListOfLength :: Gen a -> Int -> Gen [a]
genListOfLength ga 0 = return []
genListOfLength ga n = (:) <$> ga <*> genListOfLength ga (n-1)

instance Arbitrary Program where
  shrink = shrinkProgram

  arbitrary = do
    -- Generate arbitrary list of arbitrary procedures signatures.
    let typesiggen = genListOfLength arbitrary =<< choose (0,10)
    funTypes <- genListOfLength typesiggen =<< choose (0,5)

    -- Designate entrypoint procedure, with no arguments.
    let entry@(entryPoint, _) = (Ident "fun0" Nothing, [])

    -- List of all functions.
    let funs = entry:[(Ident ("fun" ++ show i) Nothing, as) | (as,i) <- zip funTypes [1..]]

    -- generate global references
    globaltypes    <- genListOfLength (elements (map Ref basetypes)) =<< choose (0,5)
    let globals    = [ (Ident ("glob" ++ show i) Nothing, t) | (t, i) <- zip globaltypes [0..] ]
    let globalrefs = map (uncurry makeStaticRef) globals

    -- Generate type, args, and body for each procedure signature.
    tab <- mfix $ \tab -> sequence
        [ do let (refs, vars) = partition (isReference . fst) $ zip as [1..]
             let inprefs      = [ makeDynamicRef (Ident ("ref" ++ show i) Nothing) t | (t,i) <- refs]
             let inpvars      = [(Ident ("var" ++ show i) Nothing, t) | (t,i) <- vars]

             -- generate a procedure body where the input parameters & global refs are in scope
             (body,_)        <- arbProc tab inpvars (inprefs ++ globalrefs) 0 =<< choose (0, 15)

             -- create (String, Type) pairs, representing the parameters to this procedure
             let params = [ (if isReference a
                             then Ident ("ref" ++ show i) Nothing
                             else Ident ("var" ++ show i) Nothing
                            , a) 
                          | (a,i) <- zip as [1..]]
             -- return (function name, parameters, body)
             return (f, params, body)
        | (f,as) <- funs]

    let procedures = Map.fromList
          [(fun, Procedure fun params bdy) | (fun, params, bdy) <- tab]

    return $ Program entryPoint procedures globals

-- | Generate a procedure body.
arbProc :: Procedures     -- ^ All procedures in the program
        -> [Variable]     -- ^ Variables in scope
        -> [Reference]    -- ^ References in scope
        -> Int            -- ^ Fresh name generator
        -> Int            -- ^ Size parameter
        -> Gen ([Stm], Int)
arbProc _ _ _ c 0          = return ([], c)
arbProc funs vars refs c n = frequency $
      [ (1, do t         <- elements basetypes
               e         <- choose (0,3) >>= arbExp t vars refs
               (name,c1) <- fresh c
               let rt     = mkReference t
               let stm    = NewRef name t e
               let ref    = makeDynamicRef name rt
               (rest, c2) <- arbProc funs vars (ref:refs) c1 (n-1)
               return (stm:rest, c2)
        )
      
      , (1, do cond      <- choose (0,3) >>= arbExp TBool vars refs
               (thn,c1)  <- arbProc funs vars refs c (n `div` 2)
               (els,c2)  <- arbProc funs vars refs c1 (n `div` 2)
               (rest,c3) <- arbProc funs vars refs c2 (n-1)
               let stm    = If cond thn els
               return (stm:rest, c3)
        )
      
      --, (1, undefined {-while-}
      --  )

      , (1, do let forkable = elements funs `suchThat` canBeCalled refs
               tofork     <- genListOfLength forkable 5 `suchThat` (not . null)
               forks       <- mapM (applyFork vars refs) tofork
               let stm      = Fork forks
               (rest,c') <- arbProc funs vars refs c (n-1)
               return (stm:rest, c')
        )
      ] ++

      (if null vars then [] else
      [ (1, do (name,t) <- elements vars
               e        <- choose (0,3) >>= arbExp t vars refs
               (rest,c') <- arbProc funs vars refs c (n-1)
               let stm   = SetLocal name t e
               return (stm:rest, c')
        )]) ++

      (if null refs then [] else
      [ (1, do r       <- elements refs
               e       <- choose (0,3) >>= arbExp (dereference $ refType r) vars refs
               (rest,c') <- arbProc funs vars refs c (n-1)
               let stm = SetRef r e
               return (stm:rest, c')
        )
      
      , (1, do r        <- elements refs
               v        <- choose (0,3) >>= arbExp (dereference $ refType r) vars refs
               (rest,c') <- arbProc funs vars refs c (n-1)
               delay     <- choose (0, 3) >>= genDelay
               let stm   = After delay r v
               return (stm:rest, c')
        )
      , (1, do refs'  <- sublistOf refs `suchThat` (not . null)
               (rest,c') <- arbProc funs vars refs c (n-1)
               let stm = Wait refs'
               return (stm:rest, c')
        )
      ])
  where
      -- | Generate a fresh name.
      fresh :: Monad m => Int -> m (Ident, Int)
      fresh c = return $ (Ident ("v" ++ show c) Nothing, c+1)

      -- | Take a procedure that should be forked and return the application of
      -- that procedure to randomly generated arguments.
      applyFork :: [Variable]
                -> [Reference]
                -> (Ident, [(Ident, Type)], [Stm])
                -> Gen (Ident, [Either SSMExp Reference])
      applyFork vars refs (n, types, _) = do
          args <- forM types $ \(_,t) ->
            if isReference t
              then do let okrefs = filter ((==) t . refType) refs
                      Right <$> elements okrefs
              else Left <$> (choose (0,3) >>= arbExp t vars refs)
          return (n, args)

      -- | Generate a delay.
      genDelay :: Int -> Gen SSMTime
      genDelay 0 = do
          unit' <- arbitrary
          delay <- Lit TUInt64 . LUInt64 <$> arbDelay unit'
          return $ SSMTime delay unit'
            where
              arbDelay :: SSMTimeUnit -> Gen Word64
              arbDelay SSMHour        = choose (1, 3)
              arbDelay SSMMinute      = choose (1, 180)
              arbDelay SSMSecond      = choose (1, 10800)
              arbDelay SSMMillisecond = choose (1, 10800000)
              arbDelay SSMMicrosecond = choose (1, 10800000000)
              arbDelay SSMNanosecond  = choose (1, 10800000000000)
      genDelay n = do
          t1 <- genDelay $ n `div` 2
          t2 <- genDelay $ n `div` 2
          oneof $ [ return $ SSMTimeAdd t1 t2
                  , return $ SSMTimeAdd (SSMTimeSub t1 t2) (SSMTime (Lit TUInt64 (LUInt64 1)) SSMNanosecond)
                  ]

      -- | Predicate that returns True if the given procedure can be forked.
      -- What determines this is what references we have in scope. If the procedure
      -- requires a reference parameter that we do not have, we can not fork it.
      canBeCalled :: [Reference] -> (Ident, [(Ident, Type)], [Stm]) -> Bool
      canBeCalled inscope (_, types, _) =
          let distinct = nub $ filter isReference $ map refType inscope
          in all (`elem` distinct) $ filter isReference $ map snd types

-- | Generate a SSMExp.
arbExp :: Type        -- ^ Type of expression to generate
       -> [Variable]  -- ^ Variables that are in scope that the expression can use
       -> [Reference] -- ^ References that are in scope that the expression can use
       -> Int         -- ^ Size parameter
       -> Gen SSMExp
arbExp t vars refs 0 = oneof $ concat [ [litGen]
                                      , varGen
                                      , if t == TBool then changedGen else []
                                      , derefGen
                                      ]
  where
    -- | Generator of SSMExp literals.
    litGen :: Gen SSMExp
    litGen = case t of
      TInt32  -> return . Lit TInt32  . LInt32  =<< choose (0, 215)
      TInt64  -> return . Lit TInt64  . LInt64  =<< choose (-55050, 55050)
      TUInt8  -> return . Lit TUInt8  . LUInt8  =<< choose (0,80)
      TUInt64 -> return . Lit TUInt64 . LUInt64 =<< choose (0, 65500)
      TBool  -> return  . Lit TBool   . LBool   =<< arbitrary
      TEvent -> return $ Lit TEvent LEvent
      _ -> error "not a base type"

    -- | Generator that returns a randomly selected variable from the set of variables.
    varGen :: [Gen SSMExp]
    varGen = [ return (Var t' n) | (n,t') <- vars, t == t']

    -- | Generator that returns applications of the @changed@ operator
    changedGen :: [Gen SSMExp]
    changedGen = if null refs
                   then []
                   else [ return $ UOpR TBool r Changed | r <- refs]

    -- | Generator that returns dereferenced references
    derefGen :: [Gen SSMExp]
    derefGen = if null refs
      then []
      else [ return $ UOpR (dereference (refType r)) r Deref
           | r <- refs, dereference (refType r) == t
           ]

arbExp t vars refs n = case t of
  TBool -> oneof $ [ do e1 <- arbExp TInt32 vars refs (n `div` 2)
                        e2 <- arbExp TInt32 vars refs (n `div` 2)
                        return $ BOp t e1 e2 OLT
                   , do typ <- elements [TInt32, TInt32, TUInt64, TBool, TEvent]
                        e1 <- arbExp typ vars refs (n `div` 2)
                        e2 <- arbExp typ vars refs (n `div` 2)
                        return $ BOp TBool e1 e2 OEQ
                   , do e1 <- arbExp TBool vars refs (n `div` 2)
                        e2 <- arbExp TBool vars refs (n `div` 2)
                        return $ BOp TBool e1 e2 OAnd
                   , do e1 <- arbExp TBool vars refs (n `div` 2)
                        e2 <- arbExp TBool vars refs (n `div` 2)
                        return $ BOp TBool e1 e2 OOr
                   , do e <- arbExp TBool vars refs (n `div` 2)
                        return $ UOpE TBool e Not
                   ]
  TEvent -> return $ Lit TEvent LEvent
  t | t `elem` [TUInt8, TUInt64] -> do
      e1 <- arbExp t vars refs (n `div` 2)
      e2 <- arbExp t vars refs (n `div` 2)
      elements [ BOp t e1 e2 OPlus
               , BOp t e1 e2 OMinus
               , BOp t e1 e2 OTimes
               {- Since division by zero can not be tolerated, we force the denominator
               to be positive by simply adding a one to the generated number. -}
--               , BOp t e1 (BOp t (e2) (one t) OPlus) ODiv
               ]
  _ ->    frequency [ (1, do e <- arbExp t vars refs (n-1)
                             return $ UOpE t e Neg
                      )
                    , (7, do e1 <- arbExp t vars refs (n `div` 2)
                             e2 <- arbExp t vars refs (n `div` 2)
                             elements [ BOp t e1 e2 OPlus
                                      , BOp t e1 e2 OMinus
                                      , BOp t e1 e2 OTimes
                                      ]
                      )
                    , (1, do e1 <- arbExp t vars refs (n `div` 2)
                             e2 <- nonNegativeLitGen t
                             elements [ BOp t e1 e2 ODiv
                                      , BOp t e1 e2 OMod
                                      ]
                    )
                    ]
  where
    {- | We can not perform a computation that is modulo zero, so we use this function
    to generate the second operand of modulo. We make sure that we only generate values
    that are non zero. -}
    nonNegativeLitGen :: Type -> Gen SSMExp
    nonNegativeLitGen t = case t of
      TUInt8  -> (Lit TUInt8 . LUInt8) <$> choose (1,255)
      TUInt64 -> (Lit TUInt64 . LUInt8) <$> choose (1, maxBound)
      TInt32  -> oneof [ (Lit TInt32 . LInt32) <$> choose (minBound, -1)
                       , (Lit TInt32 . LInt32) <$> choose (1, maxBound)
                       ]
      TInt64  -> oneof [ (Lit TInt64 . LInt64) <$> choose (minBound, -1)
                       , (Lit TInt64 . LInt64) <$> choose (1, maxBound)
                       ]
      _ -> error "nonNegativeLitGen: generator error - not a numerical type"
