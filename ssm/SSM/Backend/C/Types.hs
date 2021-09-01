module SSM.Backend.C.Types where

import           SSM.Core.Syntax

import           Language.C.Quote.GCC           ( cexp )
import qualified Language.C.Syntax             as C
import qualified SSM.Interpret.Trace           as T

import           Control.Monad.State

-- | Obtain base type for a type, i.e., unwrapping all references.
baseType :: Type -> Type
baseType (Ref t) = baseType t
baseType t       = t

-- | Maps SSM `Type` to identifier of the base type.
baseTypeId :: Type -> String
baseTypeId = typeId . baseType
 where
  typeId TInt32  = "i32"
  typeId TInt64  = "i64"
  typeId TUInt64 = "u64"
  typeId TUInt8  = "u8"
  typeId TBool   = "bool"
  typeId TEvent  = "event"

-- | Obtain formatter to show given type's base type as an integer.
intFmt :: Type -> String
intFmt = fmt . baseType
 where
  fmt TInt64  = "%ld"
  fmt TInt32  = "%d"
  fmt TBool   = "%u"
  fmt TUInt64 = "%lu"
  fmt TUInt8  = "%u"

varFmt :: (Ident, Type) -> T.VarVal
varFmt (n, t)
  | baseType t == TEvent = T.VarVal (identName n) (baseType t) T.UnitType
  | otherwise = T.VarVal (identName n) (baseType t) $ T.IntegralFmt $ intFmt t

{- | Data type that annotates a procedures body with unique line-numbers. Used to be able
to refer to individual statements in auxiliary data objects. -}
data CStm = Numbered Int Stm              -- ^ Annotate a statement with a number
          | CWhile Int SSMExp [CStm]      -- ^ While with recursive @CStm@ instead of @Stm@
          | CIf Int SSMExp [CStm] [CStm]  -- ^ If, by the same principle as @CWhile@
  deriving Show

-- | Take a procedure body and number all the statements
toCStm :: [Stm] -> [CStm]
toCStm stmts = evalState (toCStm' stmts) 0
 where
  -- | Actually number the statements and turn them into `CStm`
  toCStm' :: [Stm] -> State Int [CStm]
  toCStm' []       = return []
  toCStm' (x : xs) = case x of
    If c thn els -> do
      thn' <- toCStm' thn
      els' <- toCStm' els
      n    <- number
      xs'  <- toCStm' xs
      return $ CIf n c thn' els' : xs'
    While c bdy -> do
      bdy' <- toCStm' bdy
      n    <- number
      xs'  <- toCStm' xs
      return $ CWhile n c bdy' : xs'
    _ -> do
      n   <- number
      xs' <- toCStm' xs
      return $ Numbered n x : xs'

  -- | Generate the next unique statement number
  number :: State Int Int
  number = do
    i <- get
    put $ i + 1
    return i
