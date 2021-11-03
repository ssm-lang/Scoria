{-# OPTIONS_GHC -fplugin=SSM.Plugin #-}
module Regression.FibSpec where

import           Prelude hiding (sum)

import           Data.Word
import           SSM.Frontend.Language
import           SSM.Frontend.Box
import           SSM.Frontend.Syntax
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.SSM.Prop                 as T

{-# ANN mywait EMBED #-}
mywait :: Ref a -> SSM ()
mywait r = wait r

{-# ANN sum EMBED #-}
sum :: Ref Word64 -> Ref Word64
    -> Ref Word64 -> SSM ()
sum r1 r2 r = do
  fork [ mywait r1, mywait r2 ]
  after (secs 1) r (deref r1 + deref r2)

{-# ANN fib EMBED #-}
fib :: Exp Word64 -> Ref Word64 -> SSM ()
fib n r = do
  r1 <- var 0
  r2 <- var 0
  ifThenElse (n <. 2)
    (after (secs 1) r 1)
    (fork [ fib (n-1) r1
          , fib (n-2) r2
          , sum r1 r2 r
          ])

{-# ANN main EMBED #-}
main :: SSM ()
main = do
  r <- var 0
  fork [ fib 13 r ]

spec :: H.Spec
spec = T.correctSpec "Fib" main
