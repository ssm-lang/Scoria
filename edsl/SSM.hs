module SSM where

import AST

import Data.List.NonEmpty hiding (unzip, zip)

type SSM a = IO a -- placeholder for _some_ monad, TBD at a later time (need to talk with Koen)

(*=) :: SSM (Ref a) -> SSMExp a -> SSM SSMStm
r *= e = do
    ref <- r
    return $ Assign ref e

var :: String -> SSM (Ref a)
var = return . Ref

wait :: [Ref a] -> SSM ()
wait r = undefined

procedure :: String -> (a -> SSM Routine)
procedure = undefined

mywait :: SSM Routine
mywait = procedure "mywait" $ \(ByRef r) ->
    wait [r]

mysum :: SSM Routine
mysum = procedure "mysum" $ \(ByRef r1) (ByRef r2) (ByRef r) -> do
  --fork
  --after
  undefined

myfib :: SSM Routine
myfib = procedure "myfib" $ \(ByVal n) (ByRef r) -> do
  r1 <- var "r1" *= (0 :: SSMExp Int)
  r2 <- var "r2" *= (0 :: SSMExp Int)
  --if etc
  undefined

--ex1 :: SSM Routine
--ex1 = procedure "ex1" $ \[ByVal t, ByRef i, ByRef j] -> do
--  var "a" *= (4 :: SSMExp Int)
--  var "b" *= (3 :: SSMExp Int)
--  wait t
--  undefined