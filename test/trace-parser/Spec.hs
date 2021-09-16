import           SSM.Core.Syntax
import           SSM.Core.Type
import           SSM.Interpret.Trace
import           SSM.Interpret.TraceParser

import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H

import           Data.Either
import qualified Data.Text                     as T

import           Text.Megaparsec

main :: IO ()
main = H.hspec $ do
    H.describe "Trace parser" $ do
        runProperty $ H.prop typ prop_Parse_Type
        runProperty $ H.prop varval prop_Parse_VarVal
        runProperty $ H.prop concreteval prop_Parse_ConcreteValue
        runProperty $ H.prop event prop_Parse_Event
        runProperty $ H.prop trace prop_Parse_Trace
  where
    runProperty :: H.SpecWith a -> H.SpecWith a
    runProperty = H.modifyMaxSuccess (const 10000)

    typ :: String
    typ = "parses Type"

    varval :: String
    varval = "parses VarVal"

    concreteval :: String
    concreteval = "parses ConcreteVal"

    event :: String
    event = "parses Event"

    trace :: String
    trace = "parses Trace"

    prop_Parse_Type :: Type -> Bool
    prop_Parse_Type t = isRight $ parse pType "" $ T.pack (show t)

    prop_Parse_VarVal :: VarVal -> Bool
    prop_Parse_VarVal vv = isRight $ parse pVarVal "" $ T.pack (show vv)

    prop_Parse_ConcreteValue :: ConcreteValue -> Bool
    prop_Parse_ConcreteValue cv =
        isRight $ parse pConcreteValue "" $ T.pack (show cv)

    prop_Parse_Event :: Event -> Bool
    prop_Parse_Event e = isRight $ parse pEvent "" $ T.pack (show e)

    prop_Parse_Trace :: Trace -> Bool
    prop_Parse_Trace tr =
        tr == parseTrace (T.unlines $ Prelude.map (T.pack . show) tr)
