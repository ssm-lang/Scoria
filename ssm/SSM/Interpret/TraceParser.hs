{-# LANGUAGE OverloadedStrings #-}
module SSM.Interpret.TraceParser where

import           SSM.Core.Syntax
import           SSM.Core.Type
import           SSM.Interpret.Trace

import           Data.List.NonEmpty
import           Data.Maybe
import qualified Data.Set                      as Set
import qualified Data.Text                     as T
import           Data.Void

import           Text.Megaparsec
import qualified Text.Megaparsec.Byte.Lexer    as Lexer
import           Text.Megaparsec.Char

type Parser a = Parsec Void T.Text a

-- | Parse a single symbol
pSymbol :: T.Text -> Parser T.Text
pSymbol = Lexer.symbol pSpace

-- | Consume a single character
pChar :: Char -> Parser ()
pChar c = do
    pSpace *> char c <* pSpace
    return ()

-- | Consume space character
pSpace :: Parser ()
pSpace = do
    space
    return ()

{- | Run a parser after consuming a opening parentheses, and consuming a closing
parentheses after. -}
parens :: Parser a -> Parser a
parens p = do
    pChar '('
    a <- p
    pChar ')'
    return a

-- | Parse a single event
parseEvent :: T.Text -> Maybe Event
parseEvent t = case parse pEvent "" t of
    Left  e -> Nothing
    Right r -> Just r

-- | Parse a single event, from a string. Synonymous to @parseEvent . Data.Text.Pack@
parseEventS :: String -> Maybe Event
parseEventS = parseEvent . T.pack

-- | Parses the output found on stdout when running the C program
parseTrace :: T.Text -> Trace
parseTrace t = go $ T.lines t
  where
    go :: [T.Text] -> Trace
    go []       = []
    go (t : ts) = case parse pEvent "" t of
        Left  e -> error $ show e
        Right r -> r : go ts

{- | Parses the output found on stdout when running the C program, but works on a
@String@ instead of @Text@. Synonymous to @parseTrace . Data.Text.pack@. -}
parseTraceS :: String -> Trace
parseTraceS = parseTrace . T.pack

-- | Parse a `SSM.Internal.Trace.Event`
pEvent :: Parser Event
pEvent = choice
    [ try pActStepBegin
    , try pActActivate
    , try pActSensitize
    , pActVar
    , pDriverEventQueueStatus
    , pTerminatedOk
    , try pExhaustedMicrotick
    , try pExhaustedEventQueue
    , try pExhaustedActQueue
    , try pExhaustedMemory
    , pExhaustedPriority
    , try pCrashInvalidTime
    , try pCrashArithmeticError
    , pCrashUnforeseen
    ]
  where
    -- | Parse a `SSM.Internal.Trace.Event.ActStepBegin` value
    pActStepBegin :: Parser Event
    pActStepBegin = do
        pSymbol actStepBegin
        pChar '"'
        fun <- some alphaNumChar
        pChar '"'
        return $ ActStepBegin fun

    -- | Parse a `SSM.Internal.Trace.Event.ActActivate` value
    pActActivate :: Parser Event
    pActActivate = do
        pSymbol actActivate
        pChar '"'
        fun <- some alphaNumChar
        pChar '"'
        return $ ActActivate fun

    -- | Parse a `SSM.Internal.Trace.Event.ActVar`
    pActVar :: Parser Event
    pActVar = do
        pSymbol actVar
        varval <- parens pVarVal
        return $ ActVar varval

    -- | Parse a `SSM.Internal.Trace.Event.DriverEventQueueStatus`
    pDriverEventQueueStatus :: Parser Event
    pDriverEventQueueStatus = do
        pSymbol driverEventQueueStatus
        l <- some digitChar
        pSpace
        t <- some digitChar
        return $ DriverEventQueueStatus (read l) (read t)

    -- | Parse a `SSM.Internal.Trace.Event.ActSensitize`
    pActSensitize :: Parser Event
    pActSensitize = do
        pSymbol actSensitize
        pChar '"'
        v <- some alphaNumChar
        pChar '"'
        return $ ActSensitize v

    -- | Parse a `SSM.Internal.Trace.Event.TerminatedOk`
    pTerminatedOk :: Parser Event
    pTerminatedOk = pSymbol terminatedOk *> return TerminatedOk

    -- | Parse a `SSM.Internal.Trace.Event.ExhaustedMicrotick`
    pExhaustedMicrotick :: Parser Event
    pExhaustedMicrotick =
        pSymbol exhaustedMicrotick *> return ExhaustedMicrotick

    -- | Parse a `SSM.Internal.Trace.Event.ExhaustedActQueue`
    pExhaustedActQueue :: Parser Event
    pExhaustedActQueue = pSymbol exhaustedActQueue *> return ExhaustedActQueue

    -- | Parse a `SSM.Internal.Trace.Event.ExhaustedEventQueue`
    pExhaustedEventQueue :: Parser Event
    pExhaustedEventQueue =
        pSymbol exhaustedEventQueue *> return ExhaustedEventQueue

    -- | Parse a `SSM.Internal.Trace.Event.ExhaustedMemory`
    pExhaustedMemory :: Parser Event
    pExhaustedMemory = pSymbol exhaustedMemory *> return ExhaustedMemory

    -- | Parse a `SSM.Internal.Trace.Event.ExhaustedPriority`
    pExhaustedPriority :: Parser Event
    pExhaustedPriority = pSymbol exhaustedPriority *> return ExhaustedPriority

    -- | Parse a `SSM.Internal.Trace.Event.CrashInvalidTime`
    pCrashInvalidTime :: Parser Event
    pCrashInvalidTime = pSymbol crashInvalidTime *> return CrashInvalidTime

    -- | Parse a `SSM.Internal.Trace.Event.CrashArithmeticError`
    pCrashArithmeticError :: Parser Event
    pCrashArithmeticError =
        pSymbol crashArithmeticError *> return CrashArithmeticError

    -- | Parse a `SSM.Internal.Trace.Event.CrashUnforeseen`
    pCrashUnforeseen :: Parser Event
    pCrashUnforeseen = do
        pSymbol crashUnforeseen
        pChar '"'
        reason <- someTill alphaNumChar "\""
        return $ CrashUnforeseen reason

-- | Parse a `SSM.Interpret.Trace.VarVal`
pVarVal :: Parser VarVal
pVarVal = do
    pSymbol varVal
    pChar '"'
    varIdent <- some alphaNumChar
    pChar '"'
    t <- pType
    pSpace
    cval <- pConcreteValue
    return $ VarVal varIdent t cval

-- | Parser a `SSM.Core.Syntax.Type`
pType :: Parser Type
pType = choice [parseType, parens parseType]
  where
    -- | Parse a base type or a reference type
    parseType :: Parser Type
    parseType = choice [basetype, reftype]

    -- | Parse a reference type
    reftype :: Parser Type
    reftype = do
        pSymbol ref
        t <- pType
        return $ Ref t

    -- | Parse a base type
    basetype :: Parser Type
    basetype = do
        pChar 'T'
        rest <- some alphaNumChar
        case rest of
            "UInt8"  -> return TUInt8
            "UInt32" -> return TUInt32
            "UInt64" -> return TUInt64
            "Int32"  -> return TInt32
            "Int64"  -> return TInt64
            "Bool"   -> return TBool
            "Event"  -> return TEvent
            otherwise ->
                failure (Just $ Label $ fromJust $ nonEmpty otherwise) expected

    -- | If, after parsing a 'T', parsing failed, report these as the expected streams
    expected :: Set.Set (ErrorItem Char)
    expected = Set.fromList
        [ Label $ fromJust $ nonEmpty $ "UInt8"
        , Label $ fromJust $ nonEmpty $ "UInt32"
        , Label $ fromJust $ nonEmpty $ "UInt64"
        , Label $ fromJust $ nonEmpty $ "Int32"
        , Label $ fromJust $ nonEmpty $ "Int64"
        , Label $ fromJust $ nonEmpty $ "Bool"
        , Label $ fromJust $ nonEmpty $ "Event"
        ]

-- | Parse a `SSM.Interpret.Trace.ConcreteValue`
pConcreteValue :: Parser ConcreteValue
pConcreteValue = choice [pUnitType, try (parens pIntegralVal)]
  where
    -- | Parse `SSM.Intepret.Trace.ConcreteValue.IntegralVal`
    pIntegralVal :: Parser ConcreteValue
    pIntegralVal = do
        pSymbol integralVal
        num <- pInteger
        return $ IntegralVal num

    -- | Parse `SSM.Intepret.Trace.ConcreteValue.UnitType`
    pUnitType :: Parser ConcreteValue
    pUnitType = pSymbol unitType *> return UnitType

pInteger :: Parser Integer
pInteger = choice
    [read <$> some digitChar, (negate . read) <$> (pChar '-' *> some digitChar)]
