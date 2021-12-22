{- | This is an attempt at generating and evaluating monadic programs. -}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
module Test.SSM.QuickCheck.Generator2 where

import Test.QuickCheck

import qualified Data.Map as Map
import Data.Int ( Int32, Int64 )
import Data.Word ( Word8, Word32, Word64 )

import qualified SSM.Core as C
import qualified SSM.Language as L

import Control.Monad

type BoundVar = (String, C.Type)  -- ^ do-statement variable names

hasType :: BoundVar -> C.Type -> Bool
bv `hasType` t' = typeof bv == t'

typeof :: BoundVar -> C.Type
typeof (_, t) = C.dereference t

ofType :: [BoundVar] -> C.Type -> [BoundVar]
bvs `ofType` t = filter (flip hasType t) bvs

isSigned :: C.Type -> Bool
isSigned C.TInt32 = True
isSigned C.TInt64 = True
isSigned _        = False

includedIf :: a -> Bool -> [a]
e `includedIf` True  = [e]
_ `includedIf` False = []

data ExpAPI =
    -- * Literals
      I32     C.Type Int32          -- ^ 32bit signed integers
    | I64     C.Type Int64          -- ^ 64bit signed integers
    | U8      C.Type Word8          -- ^ 8bit unsigned integers
    | U32     C.Type Word32         -- ^ 32bit unsigned integers
    | U64     C.Type Word64         -- ^ 64bit unsigned integers
    | T       C.Type                -- ^ true
    | F       C.Type                -- ^ false
    | E       C.Type                -- ^ event
    -- * Boolean operators
    | Eq      C.Type ExpAPI ExpAPI  -- ^ Equality (==)
    | Not     C.Type ExpAPI         -- ^ Boolean negation (not)
    | And     C.Type ExpAPI ExpAPI  -- ^ Conjunction (&&)
    | Or      C.Type ExpAPI ExpAPI  -- ^ Disjunction (||)
    -- * Numeric operators
    | Add     C.Type ExpAPI ExpAPI  -- ^ Addition (+)
    | Sub     C.Type ExpAPI ExpAPI  -- ^ Subtraction (-)
    | Mul     C.Type ExpAPI ExpAPI  -- ^ Multiplication (*)
    | Div     C.Type ExpAPI ExpAPI  -- ^ Division (/)
    | Rem     C.Type ExpAPI ExpAPI  -- ^ Remainder (%)
    | Neg     C.Type ExpAPI         -- ^ Negation (neg)
    -- * Bitwise operators
    | LS      C.Type ExpAPI ExpAPI  -- ^ Left shift (<<)
    | RS      C.Type ExpAPI ExpAPI  -- ^ Right shift (>>)
    | BA      C.Type ExpAPI ExpAPI  -- ^ Bitwise and (&)
    | BO      C.Type ExpAPI ExpAPI  -- ^ Bitwise or (|)
    -- * Reference operators
    | Deref   C.Type BoundVar       -- ^ Dereference reference (deref)
    | Changed C.Type BoundVar       -- ^ changed
  deriving Show

data TimeAPI =
      NS ExpAPI  -- ^ nsecs
    | US ExpAPI  -- ^ usecs
    | MS ExpAPI  -- ^ msecs
    | S  ExpAPI  -- ^ secs
    | M  ExpAPI  -- ^ mins
    | HR ExpAPI  -- ^ hrs
  deriving Show

data StmAPI =
      Var ExpAPI                         -- ^ var
    | Assign BoundVar ExpAPI             -- ^ assign
    | Wait [BoundVar]                    -- ^ wait
    | After TimeAPI BoundVar ExpAPI      -- ^ after
    | Fork [[DoStm]]                     -- ^ fork
    | IfThen ExpAPI [DoStm]              -- ^ ifThen
    | IfThenElse ExpAPI [DoStm] [DoStm]  -- ^ ifThenElse
    | While ExpAPI [DoStm]               -- ^ while
  deriving Show

data DoStm =
      Bind   BoundVar StmAPI  -- ^ x <- stm
    | NoBind StmAPI           -- ^ stm
  deriving Show

-- * Environment management

data St = St { size    :: Int
             , counter :: Int
             , bvs     :: [BoundVar]
             }
  deriving Show

decSize :: St -> St
decSize env = env { size = size env - 1 }

withSize :: St -> Int -> St
withSize env size' = env { size = size' }

withVar :: St -> BoundVar -> St
withVar env bv = env { bvs = bv : bvs env }

noSizeLeft :: St -> Bool
noSizeLeft env = size env == 0

hasReferences :: St -> Bool
hasReferences = not . null . bvs

fresh :: St -> (String, St)
fresh env = ("v" <> show (counter env), env { counter = counter env + 1})

-- * Generate expressions

instance Arbitrary ExpAPI where
  arbitrary = do t <- basetype
                 genExp (emptySt `withSize` 3) t

genExp :: St -> C.Type -> Gen ExpAPI
genExp env C.TBool   = genBool  env
genExp env C.TEvent  = genEvent env
genExp env C.TUInt8  = genU8    env
genExp env C.TUInt32 = genU32   env
genExp env C.TUInt64 = genU64   env
genExp env C.TInt32  = genI32   env
genExp env C.TInt64  = genI64   env
genExp _ _           = error "ref type"

genNum :: St -> C.Type -> (St -> Gen ExpAPI) -> Gen ExpAPI -> Gen ExpAPI
genNum env t recu base
  | noSizeLeft env = base
  | otherwise = frequency $
    [ (1, base)
    , (1, Add t <$> recu (decSize env) <*> recu (decSize env))
    , (1, Sub t <$> recu (decSize env) <*> recu (decSize env))
    , (1, Mul t <$> recu (decSize env) <*> recu (decSize env))
    , (1, LS  t <$> recu (decSize env) <*> recu (decSize env))
    , (1, RS  t <$> recu (decSize env) <*> recu (decSize env))
    , (1, BA  t <$> recu (decSize env) <*> recu (decSize env))
    , (1, BO  t <$> recu (decSize env) <*> recu (decSize env))
    ] <>
    ((1, Neg t <$> recu (decSize env)) `includedIf` isSigned t) <>
    ((1, elements $ map (Deref t) $ bvs env `ofType` t) `includedIf`
        (hasReferences env && (not . null) (bvs env `ofType` t)))

genI32 :: St -> Gen ExpAPI
genI32 env = genNum env C.TInt32 genI32 (I64 C.TInt32 <$> arbitrary)

genI64 :: St -> Gen ExpAPI
genI64 env = genNum env C.TInt64 genI64 (I64 C.TInt64 <$> arbitrary)

genU8 :: St -> Gen ExpAPI
genU8 env = genNum env C.TUInt8 genU8 (U8 C.TUInt8 <$> arbitrary)

genU32 :: St -> Gen ExpAPI
genU32 env = genNum env C.TUInt32 genU32 (U32 C.TUInt32 <$> arbitrary)

genU64 :: St -> Gen ExpAPI
genU64 env = genNum env C.TUInt64 genU64 (U64 C.TUInt64 <$> arbitrary)

genEvent :: St -> Gen ExpAPI
genEvent env = oneof $
  [ return $ E C.TEvent
  ] <>
  (elements $ map (Deref C.TEvent) $ bvs env `ofType` C.TEvent) `includedIf`
    (hasReferences env && (not . null) (bvs env `ofType` C.TEvent))

genBool :: St -> Gen ExpAPI
genBool env
  | noSizeLeft env = oneof [return $ T C.TBool, return $ F C.TBool]
  | otherwise =
    frequency $
      [ (1, oneof [return $ T C.TBool, return $ F C.TBool])
      , (1, Not C.TBool <$> genBool (decSize env))
      , (1, And C.TBool <$> genBool (decSize env) <*> genBool (decSize env))
      , (1, Or  C.TBool <$> genBool (decSize env) <*> genBool (decSize env))
      -- case for ==, Eq
      ] <>
      ((1, elements $ map (Changed C.TBool) $ bvs env) `includedIf` hasReferences env) <>
      ((1, elements $ map (Deref C.TBool) $ bvs env `ofType` C.TBool) `includedIf`
        (hasReferences env && (not . null) (bvs env `ofType` C.TBool)))

-- * Generate time delays

genTime :: St -> Gen TimeAPI
genTime env = do
  d <- genU64 $ env `withSize` 2
  elements [ NS d, US d, MS d, S d, M d, HR d ]

-- * Generate statements

basetype :: Gen C.Type
basetype =
  elements [ C.TUInt8, C.TUInt32, C.TUInt64, C.TInt32, C.TInt64, C.TBool, C.TEvent]

genStmts :: St -> Gen ([DoStm], St)
genStmts env
  | noSizeLeft env = return ([], env)
  | otherwise      = do
    (x,  env')  <- genStm env
    (xs, env'') <- genStmts env'
    return (x:xs, env'')

genStm :: St -> Gen (DoStm, St)
genStm env = frequency $ base <> envSensitive
  where
    -- | These generators can always run, regardless of the environment
    base :: [(Int, Gen (DoStm, St))]
    base = [ -- var
             (3, do t <- basetype
                    e <- genExp (env `withSize` 2) t
                    let (id, env') = fresh env
                        bv         = (id, C.mkReference t)
                        stm        = Var e
                    return (Bind bv stm, decSize $ env' `withVar` bv)
             )
             -- ifThen
           , (1, do test     <- genExp (env `withSize` 2) C.TBool
                    (thn, _) <- genStmts (decSize env)
                    return (NoBind $ IfThen test thn, decSize env)
             )
             -- ifThenElse
           , (1, do test     <- genExp (env `withSize` 2) C.TBool
                    (thn, _) <- genStmts (decSize env)
                    (els, _) <- genStmts (decSize env)
                    return (NoBind $ IfThenElse test thn els, decSize env)
             )
             -- while
           , (1, do test      <- genExp (env `withSize` 2) C.TBool
                    (thn, _)  <- genStmts (decSize env)
                    return (NoBind $ While test thn, decSize env)
             )
          --  , (1, do nf     <- chooseInt (1,2)
          --           bodies <- (fst . unzip) <$> replicateM nf (genStmts (env `withSize` 4))
          --           return (NoBind $ Fork bodies, decSize env)
          --    )
           ]

    -- | These generators can only run if the environment contains any bound variables
    envSensitive :: [(Int, Gen (DoStm, St))]
    envSensitive
      | hasReferences env =
        [ -- assign
          (1, do r <- elements $ bvs env
                 e <- genExp (env `withSize` 2) (typeof r)
                 return (NoBind $ Assign r e, decSize env)
          )
        , (1, do rs <- listOf $ elements $ bvs env
                 return (NoBind $ Wait rs, decSize env)
          )
        , (1, do r <- elements $ bvs env
                 t <- genTime env
                 e <- genExp (env `withSize` 2) (typeof r)
                 return (NoBind $ After t r e, decSize env)
          )
        ]
      | otherwise = []

emptySt :: St
emptySt = St 10 0 []

instance {-# OVERLAPPING #-} Arbitrary ([DoStm], St) where
  arbitrary = genStmts emptySt

-- * Evaluation

eval :: [DoStm] -> L.SSM ()
eval stmts = undefined

data Witness a where
  Int32Witness  :: Witness Int32
  Int64Witness  :: Witness Int64
  UInt8Witness  :: Witness Word8
  UInt32Witness :: Witness Word32
  UInt64Witness :: Witness Word64
  BoolWitness   :: Witness Bool
  EventWitness  :: Witness ()

data HRef where
  HRef :: C.SSMType a => L.Ref a -> Witness a -> HRef

data HExp where
  HExp :: C.SSMType a => L.Exp a -> Witness a -> HExp

data EvalSt = EvalSt
  { vars :: Map.Map BoundVar HRef
  }

withHVar :: EvalSt -> (BoundVar, HRef) -> EvalSt
withHVar env (bv, hr) = env { vars = Map.insert bv hr (vars env) }

lookupVar :: EvalSt -> BoundVar -> HRef
lookupVar env bv = case Map.lookup bv (vars env) of
  Just href -> href
  Nothing   -> error $ "unbound bv " <> show bv

class WitnessOf a where
  witness :: Witness a

instance WitnessOf Int32 where
  witness = Int32Witness

instance WitnessOf Int64 where
  witness = Int64Witness

instance WitnessOf Word8 where
  witness = UInt8Witness

instance WitnessOf Word32 where
  witness = UInt32Witness

instance WitnessOf Word64 where
  witness = UInt64Witness

instance WitnessOf Bool where
  witness = BoolWitness

instance WitnessOf () where
  witness = EventWitness

-- * Evaluate expressions

not' :: HExp -> HExp
not' (HExp e BoolWitness) = HExp (L.not' e) witness
not' _ = error "not' applied to argument of wrong type"

bin :: WitnessOf a => HExp -> HExp -> Witness a -> (L.Exp a -> L.Exp a -> L.Exp a) -> HExp
bin (HExp e1 Int32Witness)  (HExp e2 Int32Witness)  Int32Witness  f = HExp (f e1 e2) (witness @Int32)
bin (HExp e1 Int64Witness)  (HExp e2 Int64Witness)  Int64Witness  f = HExp (f e1 e2) (witness @Int64)
bin (HExp e1 UInt8Witness)  (HExp e2 UInt8Witness)  UInt8Witness  f = HExp (f e1 e2) (witness @Word8)
bin (HExp e1 UInt32Witness) (HExp e2 UInt32Witness) UInt32Witness f = HExp (f e1 e2) (witness @Word32)
bin (HExp e1 UInt64Witness) (HExp e2 UInt64Witness) UInt64Witness f = HExp (f e1 e2) (witness @Word64)
bin (HExp e1 BoolWitness)   (HExp e2 BoolWitness)   BoolWitness   f = HExp (f e1 e2) (witness @Bool)
bin (HExp e1 EventWitness)  (HExp e2 EventWitness)  EventWitness  f = HExp (f e1 e2) (witness @())
bin _ _ _ _ = error "bin applied to arguments of wrong type"

add' :: HExp -> HExp -> HExp
add' (HExp e1 Int32Witness)  (HExp e2 Int32Witness)  = HExp (e1 + e2) Int32Witness
add' (HExp e1 Int64Witness)  (HExp e2 Int64Witness)  = HExp (e1 + e2) Int64Witness
add' (HExp e1 UInt8Witness)  (HExp e2 UInt8Witness)  = HExp (e1 + e2) UInt8Witness
add' (HExp e1 UInt32Witness) (HExp e2 UInt32Witness) = HExp (e1 + e2) UInt32Witness
add' (HExp e1 UInt64Witness) (HExp e2 UInt64Witness) = HExp (e1 + e2) UInt64Witness

sub' :: HExp -> HExp -> HExp
sub' (HExp e1 Int32Witness)  (HExp e2 Int32Witness)  = HExp (e1 - e2) Int32Witness
sub' (HExp e1 Int64Witness)  (HExp e2 Int64Witness)  = HExp (e1 - e2) Int64Witness
sub' (HExp e1 UInt8Witness)  (HExp e2 UInt8Witness)  = HExp (e1 - e2) UInt8Witness
sub' (HExp e1 UInt32Witness) (HExp e2 UInt32Witness) = HExp (e1 - e2) UInt32Witness
sub' (HExp e1 UInt64Witness) (HExp e2 UInt64Witness) = HExp (e1 - e2) UInt64Witness

mul' :: HExp -> HExp -> HExp
mul' (HExp e1 Int32Witness)  (HExp e2 Int32Witness)  = HExp (e1 * e2) Int32Witness
mul' (HExp e1 Int64Witness)  (HExp e2 Int64Witness)  = HExp (e1 * e2) Int64Witness
mul' (HExp e1 UInt8Witness)  (HExp e2 UInt8Witness)  = HExp (e1 * e2) UInt8Witness
mul' (HExp e1 UInt32Witness) (HExp e2 UInt32Witness) = HExp (e1 * e2) UInt32Witness
mul' (HExp e1 UInt64Witness) (HExp e2 UInt64Witness) = HExp (e1 * e2) UInt64Witness

div' :: HExp -> HExp -> HExp
div' (HExp e1 Int32Witness)  (HExp e2 Int32Witness)  = HExp (e1 L./. e2) Int32Witness
div' (HExp e1 Int64Witness)  (HExp e2 Int64Witness)  = HExp (e1 L./. e2) Int64Witness
div' (HExp e1 UInt8Witness)  (HExp e2 UInt8Witness)  = HExp (e1 L./. e2) UInt8Witness
div' (HExp e1 UInt32Witness) (HExp e2 UInt32Witness) = HExp (e1 L./. e2) UInt32Witness
div' (HExp e1 UInt64Witness) (HExp e2 UInt64Witness) = HExp (e1 L./. e2) UInt64Witness

rem' :: HExp -> HExp -> HExp
rem' (HExp e1 Int32Witness)  (HExp e2 Int32Witness)  = HExp (e1 L.%. e2) Int32Witness
rem' (HExp e1 Int64Witness)  (HExp e2 Int64Witness)  = HExp (e1 L.%. e2) Int64Witness
rem' (HExp e1 UInt8Witness)  (HExp e2 UInt8Witness)  = HExp (e1 L.%. e2) UInt8Witness
rem' (HExp e1 UInt32Witness) (HExp e2 UInt32Witness) = HExp (e1 L.%. e2) UInt32Witness
rem' (HExp e1 UInt64Witness) (HExp e2 UInt64Witness) = HExp (e1 L.%. e2) UInt64Witness

ls' :: HExp -> HExp -> HExp
ls' (HExp e1 Int32Witness)  (HExp e2 Int32Witness)  = HExp (e1 L.<<. e2) Int32Witness
ls' (HExp e1 Int64Witness)  (HExp e2 Int64Witness)  = HExp (e1 L.<<. e2) Int64Witness
ls' (HExp e1 UInt8Witness)  (HExp e2 UInt8Witness)  = HExp (e1 L.<<. e2) UInt8Witness
ls' (HExp e1 UInt32Witness) (HExp e2 UInt32Witness) = HExp (e1 L.<<. e2) UInt32Witness
ls' (HExp e1 UInt64Witness) (HExp e2 UInt64Witness) = HExp (e1 L.<<. e2) UInt64Witness

rs' :: HExp -> HExp -> HExp
rs' (HExp e1 Int32Witness)  (HExp e2 Int32Witness)  = HExp (e1 L.>>. e2) Int32Witness
rs' (HExp e1 Int64Witness)  (HExp e2 Int64Witness)  = HExp (e1 L.>>. e2) Int64Witness
rs' (HExp e1 UInt8Witness)  (HExp e2 UInt8Witness)  = HExp (e1 L.>>. e2) UInt8Witness
rs' (HExp e1 UInt32Witness) (HExp e2 UInt32Witness) = HExp (e1 L.>>. e2) UInt32Witness
rs' (HExp e1 UInt64Witness) (HExp e2 UInt64Witness) = HExp (e1 L.>>. e2) UInt64Witness

ba' :: HExp -> HExp -> HExp
ba' (HExp e1 Int32Witness)  (HExp e2 Int32Witness)  = HExp (e1 L..&. e2) Int32Witness
ba' (HExp e1 Int64Witness)  (HExp e2 Int64Witness)  = HExp (e1 L..&. e2) Int64Witness
ba' (HExp e1 UInt8Witness)  (HExp e2 UInt8Witness)  = HExp (e1 L..&. e2) UInt8Witness
ba' (HExp e1 UInt32Witness) (HExp e2 UInt32Witness) = HExp (e1 L..&. e2) UInt32Witness
ba' (HExp e1 UInt64Witness) (HExp e2 UInt64Witness) = HExp (e1 L..&. e2) UInt64Witness

bo' :: HExp -> HExp -> HExp
bo' (HExp e1 Int32Witness)  (HExp e2 Int32Witness)  = HExp (e1 L..|. e2) Int32Witness
bo' (HExp e1 Int64Witness)  (HExp e2 Int64Witness)  = HExp (e1 L..|. e2) Int64Witness
bo' (HExp e1 UInt8Witness)  (HExp e2 UInt8Witness)  = HExp (e1 L..|. e2) UInt8Witness
bo' (HExp e1 UInt32Witness) (HExp e2 UInt32Witness) = HExp (e1 L..|. e2) UInt32Witness
bo' (HExp e1 UInt64Witness) (HExp e2 UInt64Witness) = HExp (e1 L..|. e2) UInt64Witness

eq' :: HExp -> HExp -> HExp
eq' (HExp e1 Int32Witness)  (HExp e2 Int32Witness)  = HExp (e1 L.==. e2) BoolWitness
eq' (HExp e1 Int64Witness)  (HExp e2 Int64Witness)  = HExp (e1 L.==. e2) BoolWitness
eq' (HExp e1 UInt8Witness)  (HExp e2 UInt8Witness)  = HExp (e1 L.==. e2) BoolWitness
eq' (HExp e1 UInt32Witness) (HExp e2 UInt32Witness) = HExp (e1 L.==. e2) BoolWitness
eq' (HExp e1 UInt64Witness) (HExp e2 UInt64Witness) = HExp (e1 L.==. e2) BoolWitness
eq' (HExp e1 BoolWitness)   (HExp e2 BoolWitness)   = HExp (e1 L.==. e2) BoolWitness
eq' (HExp e1 EventWitness)  (HExp e2 EventWitness)  = HExp (e1 L.==. e2) BoolWitness

neg' :: HExp -> HExp
neg' (HExp e1 Int32Witness) = HExp (L.neg e1) Int32Witness
neg' (HExp e1 Int64Witness) = HExp (L.neg e1) Int64Witness

deref' :: HRef -> HExp
deref' (HRef r w) = HExp (L.deref r) w

changed' :: HRef -> HExp
changed' (HRef r _) = HExp (L.changed r) BoolWitness

evalExp :: EvalSt -> ExpAPI -> HExp
evalExp env e = case e of
  -- literals
  I32     t i     -> HExp (L.i32 i) witness
  I64     t i     -> HExp (L.i64 i) witness
  U8      t w     -> HExp (L.u8 w) witness
  U32     t w     -> HExp (L.u32 w) witness
  U64     t w     -> HExp (L.u64 w) witness
  T       t       -> HExp L.true witness
  F       t       -> HExp L.false witness
  E       t       -> HExp L.event witness
  -- unary operators
  Not     t e1    -> not' (evalExp env e1)
  Neg     t e1    -> neg' (evalExp env e1)
  Deref   t bv    -> deref' (lookupVar env bv)
  Changed t bv    -> changed' (lookupVar env bv)
  -- binary operators
  And     t e1 e2 -> bin (evalExp env e1) (evalExp env e2) witness (L.&&.)
  Or      t e1 e2 -> bin (evalExp env e1) (evalExp env e2) witness (L.||.)
  Add     t e1 e2 -> add' (evalExp env e1) (evalExp env e2)
  Sub     t e1 e2 -> sub' (evalExp env e1) (evalExp env e2)
  Mul     t e1 e2 -> mul' (evalExp env e1) (evalExp env e2)
  Div     t e1 e2 -> div' (evalExp env e1) (evalExp env e2)
  Rem     t e1 e2 -> rem' (evalExp env e1) (evalExp env e2)
  LS      t e1 e2 -> ls' (evalExp env e1) (evalExp env e2)
  RS      t e1 e2 -> rs' (evalExp env e1) (evalExp env e2)
  BA      t e1 e2 -> ba' (evalExp env e1) (evalExp env e2)
  BO      t e1 e2 -> bo' (evalExp env e1) (evalExp env e2)
  Eq      t e1 e2 -> bo' (evalExp env e1) (evalExp env e2)

-- * Evaluate statements

var' :: HExp -> L.SSM HRef
var' (HExp e w) = (flip HRef w) <$> L.var e

evalStm :: EvalSt -> DoStm -> (L.SSM (), EvalSt)
evalStm env (Bind bv (Var e)) =
  let e' = evalExp env e
  r <- var' e'
  undefined
evalStm env (NoBind stm) = case stm of
  Assign bv e          -> undefined
  Wait bvs             -> undefined
  After t bv e         -> undefined
  Fork bodies          -> undefined
  IfThen c thn         -> undefined
  IfThenElse c thn els -> undefined
  While c thn          -> undefined


-- data StmAPI =
--       Var ExpAPI                         -- ^ var
--     | Assign BoundVar ExpAPI             -- ^ assign
--     | Wait [BoundVar]                    -- ^ wait
--     | After TimeAPI BoundVar ExpAPI      -- ^ after
--     | Fork [[DoStm]]                     -- ^ fork
--     | IfThen ExpAPI [DoStm]              -- ^ ifThen
--     | IfThenElse ExpAPI [DoStm] [DoStm]  -- ^ ifThenElse
--     | While ExpAPI [DoStm]               -- ^ while
--   deriving Show

-- data DoStm =
--       Bind   BoundVar StmAPI  -- ^ x <- stm
--     | NoBind StmAPI           -- ^ stm
--   deriving Show
