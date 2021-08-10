{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImplicitParams #-}
module SSM.TestProgram where

import           SSM.Compile
import           SSM.Core.Syntax
import           SSM.Interpret
import           SSM.Language
import           SSM.Pretty

import           Data.Int
import           Data.List               hiding ( cycle )
import           Data.Word

import           Prelude                 hiding ( cycle )

{- Example 1 -}
{- This is a 'nice' example. It does nothing weird, and there is no host-language
staging involved. To monomorphise this it is enough to prepend the declared names
with type information. -}

{- defaultValue has type Exp a, it's just meant to make the example more interesting
by performing an assignment, which was previously impossible because we couldn't
do type-specialized operations in polymorphic procedures. -}
setDefault :: forall a . DefSSMExp a => Ref a -> SSM ()
setDefault = box "setDefault" ["r"] $ \r -> do
    r <~ defaultValue @a

program :: SSM ()
program = boxNullary "program" $ do
    int32 <- var (1 :: Exp Int32)
    int64 <- var (1 :: Exp Int64)
    bool  <- var true'
    fork [setDefault int32, setDefault int64, setDefault bool]



{- Example 2 -}
{- This example is a little more devious. We have two distinct procedures fun1 and fun2,
but they both declare a local procedure with the same name, fun. To monomorphise this I
need to inspect the procedure bodies. If they are distinct, one is renamed with a fresh
suffix and the call site changed to call this 'new' procedure instead. -}

fun1 :: Ref Int32 -> SSM ()
fun1 = box "fun1" ["x"] $ \x -> do
    fork [fun x]
  where
    fun :: Ref Int32 -> SSM ()
    fun = box "fun" ["x"] $ \x -> do
        x <~ (0 :: Exp Int32)

fun2 :: Ref Int32 -> SSM ()
fun2 = box "fun2" ["x"] $ \x -> do
    fork [fun x]
  where
    fun :: Ref Int32 -> SSM ()
    fun = box "fun" ["x"] $ \x -> do
        x <~ (1 :: Exp Int32)

testprogram :: SSM ()
testprogram = boxNullary "testprogram" $ do
    int32 <- var (5 :: Exp Int32)
    fork [fun1 int32, fun2 int32]



{- Example 3 -}
{- This example is tricky because here we don't directly declare two different procedures
with the same name, but rather we employ host language features to produce distinct
procedure bodies. The first argument to fun3 exists in the host language, but still it
will influence the embedded language by showing up in the procedure body. The fix for
this is the same as the one for example 2. -}

fun3
    :: forall a
     . (Num a, Ord a, SSMType a, DefSSMExp a, FromLiteral a)
    => a
    -> Ref a
    -> SSM ()
fun3 n = box "fun3" ["r"] $ \r -> do
    if n < 2 then r <~ defaultValue @a else r <~ (defaultValue @a + 1)

testprogram2 :: SSM ()
testprogram2 = boxNullary "testprogram2" $ do
    r <- var (5 :: Exp Int32)
    fork [fun3 1 r, fun3 2 r, fun3 3 r]




{- Example 4 -}

{- | Produce an output pulse for a specified duration every time the reference @i@
is triggered -}
oneShot :: Ref a -> Ref b -> Exp Word64 -> Exp b -> Exp b -> SSM ()
oneShot =
    box "oneShot" ["i", "o", "duration", "high", "low"]
        $ \i o duration high low -> do
              while' true' $ do
                  wait [i]
                  o <~ high
                  after duration o low

binaryOneShot :: Exp Word64 -> Ref () -> Ref Bool -> SSM ()
binaryOneShot duration i o = oneShot i o duration true' false'


{- Example 5 -}

-- | supervisor to allocate resource and apply 'consumers'
supervisor :: forall a . DefSSMExp a => [(Ref a -> SSM ())] -> SSM ()
supervisor procs = boxNullary "supervisor" $ do
    r <- var $ defaultValue @a
    fork $ map ($ r) procs

-- 3 different consumer processes, that uses the resource
client1 :: Ref Int32 -> SSM ()
client1 = box "client1" ["r"] $ \r -> do
    r <~ int32 5

client2 :: Ref Int32 -> SSM ()
client2 = box "client2" ["r"] $ \r -> do
    r <~ int32 10

client3 :: Ref Bool -> SSM ()
client3 = box "client3" ["r"] $ \r -> do
    r <~ true'

testprogram3 :: SSM ()
testprogram3 = boxNullary "testprogram3" $ do
    fork [supervisor [client1, client2], supervisor [client3]]

{- Example 6 -}


alternate :: Ref a -> Exp a -> Exp a -> Exp Word64 -> SSM ()
alternate r e1 e2 d = fork [ alternateProcess r e1 e2 d ]
  where
      alternateProcess :: Ref a -> Exp a -> Exp a -> Exp Word64 -> SSM ()
      alternateProcess = box "alternateProcess" ["r","e1","e2","d"] $ \r e1 e2 d -> do
          while' true' $ do
              r <~ e1
              delay d
              r <~ e2
              delay d

delay :: Exp Word64 -> SSM ()
delay t = fork [delayProcedure t]
  where
    delayProcedure :: Exp Word64 -> SSM ()
    delayProcedure = box "delayProcedure" ["delay"] $ \delay -> do
        x <- var event'
        after delay x event'
        wait [x]

testprogram4 :: SSM ()
testprogram4 = boxNullary "testprogram4" $ do
    r <- var (1 :: Exp Int32)
    x <- var true'
    alternate r 2 3 50
    alternate x false' true' 50





















{-
cycle :: Ref a -> [Exp a] -> Exp Word64 -> SSM ()
cycle r xs d = fork [cycleProcedure]
  where
    {- here is an error - the reference and expressions are not passed in as parameters.
    We need supports for lists before we can write this combinator. -}
    cycleProcedure :: SSM ()
    cycleProcedure = boxNullary "cycleProcedure" $ do
        while' true' $ do
          sequence_ $ intersperse (delay d) $ map ((<~) r) xs
          delay d

delay :: Exp Word64 -> SSM ()
delay t = fork [delayProcedure t]
  where
    delayProcedure :: Exp Word64 -> SSM ()
    delayProcedure = box "delayProcedure" ["delay"] $ \delay -> do
        x <- var event'
        after delay x event'
        wait [x]

testprogram4 :: SSM ()
testprogram4 = boxNullary "testprogram4" $ do
    r <- var (0 :: Exp Int32)
    x <- var true'
    cycle r [1, 2, 3, 4] 50
    cycle x [false', true'] 50
-}







class DefSSMExp a where
    defaultValue :: Exp a

instance DefSSMExp Int32 where
    defaultValue = int32 0

instance DefSSMExp Int64 where
    defaultValue = int64 0

instance DefSSMExp Bool where
    defaultValue = false'
