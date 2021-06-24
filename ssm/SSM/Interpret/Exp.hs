module SSM.Interpret.Exp
    ( -- * Evaluating an expression
      eval

      -- * Escape hatch to go from the embedded expressions to Haskell expressions
    , getInt32
    , getInt64
    , getUInt8
    , getUInt64
    , getBool
    ) where

import SSM.Interpret.Internal
import SSM.Core.LowSyntax

import qualified Data.Map as Map

import Data.Int
import Data.Word
import Data.STRef.Lazy

import Control.Monad.State.Lazy

-- | Evaluate an SSM expression.
eval :: SSMExp -> Interp s SSMExp
eval e = do
    p <- gets process
    case e of
        Var _ n -> case Map.lookup n (variables p) of
            Just r -> do 
                v <- lift $ lift $ (readSTRef . \(x,_,_,_,_) -> x) =<< readSTRef r
                eval v
            Nothing -> error $ "interpreter error - variable " ++ n ++ " not found in current process"
        Lit _ l -> return e
        UOpR _ r op -> case op of
            Changed -> wasWritten $ fst r
        UOpE _ e Neg -> do
            e' <- eval e
            return $ neg e'
        BOp TBool e1 e2 op -> do
            l1 <- eval e1
            l2 <- eval e2
            case op of
                OLT -> return $ lessthan l1 l2
                OEQ -> return $ equals l1 l2
        BOp _ e1 e2 op -> do
            i1 <- eval e1
            i2 <- eval e2
            case op of
                OPlus  -> return $ addition i1 i2
                OMinus -> return $ SSM.Interpret.Exp.subtract i1 i2
                OTimes -> return $ multiply i1 i2

neg :: SSMExp -> SSMExp
neg (Lit _ (LInt32 i)) = Lit TInt32 $ LInt32 (-i)
neg (Lit _ (LInt64 i)) = Lit TInt64 $ LInt64 (-i)

lessthan :: SSMExp -> SSMExp -> SSMExp
lessthan (Lit _ (LInt32 i1))   (Lit _ (LInt32 i2)) = Lit TBool $ LBool $ i1 < i2
lessthan (Lit _ (LInt64 i1)) (Lit _ (LInt64 i2))   = Lit TBool $ LBool $ i1 < i2
lessthan (Lit _ (LUInt64 i1)) (Lit _ (LUInt64 i2)) = Lit TBool $ LBool $ i1 < i2
lessthan (Lit _ (LUInt8 i1)) (Lit _ (LUInt8 i2))   = Lit TBool $ LBool $ i1 < i2
lessthan _ _ = error "can only order numerical values"

equals :: SSMExp -> SSMExp -> SSMExp
equals (Lit _ (LInt32 i1))   (Lit _ (LInt32 i2)) = Lit TBool $ LBool $ i1 == i2
equals (Lit _ (LInt64 i1)) (Lit _ (LInt64 i2))   = Lit TBool $ LBool $ i1 == i2
equals (Lit _ (LUInt64 i1)) (Lit _ (LUInt64 i2)) = Lit TBool $ LBool $ i1 == i2
equals (Lit _ (LUInt8 i1)) (Lit _ (LUInt8 i2))   = Lit TBool $ LBool $ i1 == i2
equals (Lit _ (LBool b1))  (Lit _ (LBool b2))    = Lit TBool $ LBool $ b1 == b2

addition :: SSMExp -> SSMExp -> SSMExp
addition (Lit _ (LInt32 i1))   (Lit _ (LInt32 i2)) = Lit TInt32  $ LInt32  $ i1 + i2
addition (Lit _ (LInt64 i1)) (Lit _ (LInt64 i2))   = Lit TInt64  $ LInt64  $ i1 + i2
addition (Lit _ (LUInt64 i1)) (Lit _ (LUInt64 i2)) = Lit TUInt64 $ LUInt64 $ i1 + i2
addition (Lit _ (LUInt8 i1)) (Lit _ (LUInt8 i2))   = Lit TUInt8  $ LUInt8  $ i1 + i2
addition _ _ = error "can only add numerical values"

subtract :: SSMExp -> SSMExp -> SSMExp
subtract (Lit _ (LInt32 i1))   (Lit _ (LInt32 i2)) = Lit TInt32  $ LInt32  $ i1 - i2
subtract (Lit _ (LInt64 i1)) (Lit _ (LInt64 i2))   = Lit TInt64  $ LInt64  $ i1 - i2
subtract (Lit _ (LUInt64 i1)) (Lit _ (LUInt64 i2)) = Lit TUInt64 $ LUInt64 $ i1 - i2
subtract (Lit _ (LUInt8 i1)) (Lit _ (LUInt8 i2))   = Lit TUInt8  $ LUInt8  $ i1 - i2
subtract _ _ = error "can only subtract numerical values"

multiply :: SSMExp -> SSMExp -> SSMExp
multiply (Lit _ (LInt32 i1))   (Lit _ (LInt32 i2)) = Lit TInt32  $ LInt32  $ i1 * i2
multiply (Lit _ (LInt64 i1)) (Lit _ (LInt64 i2))   = Lit TInt64  $ LInt64  $ i1 * i2
multiply (Lit _ (LUInt64 i1)) (Lit _ (LUInt64 i2)) = Lit TUInt64 $ LUInt64 $ i1 * i2
multiply (Lit _ (LUInt8 i1)) (Lit _ (LUInt8 i2))   = Lit TUInt8  $ LUInt8  $ i1 * i2
multiply _ _ = error "can only multiply numerical values"

{- | Retrieve a Haskell Int32 from an expression. Will crash if the expression
is not an Int32. -}
getInt32 :: SSMExp -> Int32
getInt32 (Lit _ (LInt32 i)) = i
getInt32 e                = error $ "not an integer: " ++ show e

{- | Retrieve a Haskell Int64 from an expression. Will crash if the expression
is not an Int64. -}
getInt64 :: SSMExp -> Int64
getInt64 (Lit _ (LInt64 i)) = i
getInt64 e                  = error $ "not an integer: " ++ show e

{- | Retrieve a Haskell Word8 from an expression. Will crash if the expression
is not an Word8. -}
getUInt8 :: SSMExp -> Word8
getUInt8 (Lit _ (LUInt8  i)) = i
getUInt8 e                   = error $ "not an integer: " ++ show e

{- | Retrieve a Haskell Word64 from an expression. Will crash if the expression
is not an Word64. -}
getUInt64 :: SSMExp -> Word64
getUInt64 (Lit _ (LUInt64 i)) = i
getUInt64 e                   = error $ "not an integer: " ++ show e

{- | Retrieve a Haskell Bool from an expression. Will crash if the expression
is not an Bool. -}
getBool :: SSMExp -> Bool
getBool (Lit _ (LBool b)) = b
getBool e                 = error $ "not a boolean: " ++ show e