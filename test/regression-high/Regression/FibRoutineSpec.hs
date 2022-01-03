{-# OPTIONS_GHC -fplugin=SSM.Plugin -fplugin-opt=SSM.Plugin:mode=routine #-}
module Regression.FibRoutineSpec where

import           Prelude hiding (sum)

import SSM.Frontend.Compile
import           Data.Word
import           SSM.Frontend.Language
import           SSM.Frontend.Box
import           SSM.Frontend.Syntax
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.SSM.Prop                 as T

mywait :: Ref a -> SSM ()
mywait r = routine $ wait r

sum :: Ref Word64 -> Ref Word64
    -> Ref Word64 -> SSM ()
sum r1 r2 r = routine $ do
  fork [ mywait r1, mywait r2 ]
  after (secs 1) r (deref r1 + deref r2)

fib :: Exp Word64 -> Ref Word64 -> SSM ()
fib n r = routine $ do
  r1 <- var 0
  r2 <- var 0
  ifThenElse (n <. 2)
    (after (secs 1) r 1)
    (fork [ fib (n-1) r1
          , fib (n-2) r2
          , sum r1 r2 r
          ])

main :: SSM ()
main = routine $ do
  r <- var 0
  fork [ fib 13 r ]

program :: Compile backend ()
program = schedule main

spec :: H.Spec
spec = T.correctSpec "FibRoutine" (toProgram program)
