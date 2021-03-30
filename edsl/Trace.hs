{-# LANGUAGE OverloadedStrings #-}
module Trace where

import Data.Text
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

import Core hiding (Result)

type Parser a = Parsec Void Text a

data OutputEntry = Instant Int Int
                 | Event Int SSMExp
                 | Result String SSMExp
  deriving (Show, Eq)

type Output = [OutputEntry]

testoutput = Prelude.unlines [ "event 0 value int 0"
                             , "event 1 value bool 1"
                             , "event 2 value int 1"
                             , "now 3 eventqueuesize 3"
                             , "now 5 eventqueuesize 5"
                             , "event 6 value int 7"
                             , "result x int 1"
                             , "result y bool 0"
                             , "result z bool 1"
                             ]

mkTrace :: String -> Output
mkTrace inp = fromRight $ parse pTrace "" (pack inp)
  where
      fromRight :: Show a => Either a b -> b
      fromRight (Left a)  = error $ "parsing of test results failed: " ++ show a
      fromRight (Right b) = b

pTrace :: Parser Output
pTrace = many $ choice [pEvent, pInstant, pResult]

pEvent :: Parser OutputEntry
pEvent = do
    pSpace
    pSymbol "event"
    pSpace
    num <- Lexer.lexeme pSpace Lexer.decimal
    pSpace
    pSymbol "value"
    pSpace
    res <- pRes
    return $ Event num res

pInstant :: Parser OutputEntry
pInstant = do
    now <- pNow
    eventqueuesize <- pEventqueuesize
    return $ Instant now eventqueuesize

pResult :: Parser OutputEntry
pResult = do
    pSymbol "result"
    pSpace
    refname <- pIdent
    pSpace
    val <- pRes
    return $ Result refname val

pIdent :: Parser String
pIdent = do
    a <- letterChar 
    as <- many $ choice [letterChar, digitChar, char '_']
    pSpace
    return $ a:as

pRes :: Parser SSMExp
pRes = do
    -- add more variants here as we add more types
    choice [pInt, pBool]
  where
      pInt :: Parser SSMExp
      pInt = do
          pSpace
          pSymbol "int"
          pSpace
          Lit TInt . LInt <$> Lexer.lexeme pSpace Lexer.decimal

      pBool :: Parser SSMExp
      pBool = do
          pSpace
          pSymbol "bool"
          pSpace
          b <- Lexer.lexeme pSpace Lexer.decimal
          case b of
              0 -> return $ Lit TBool $ LBool False
              _ -> return $ Lit TBool $ LBool True

pEventqueuesize :: Parser Int
pEventqueuesize = do
    pSpace
    pSymbol "eventqueuesize"
    pSpace
    num <- Lexer.lexeme pSpace Lexer.decimal
    pSpace
    return num

pNow :: Parser Int
pNow = do
    pSymbol "now"
    pSpace
    num <- Lexer.lexeme pSpace Lexer.decimal 
    pSpace
    return num

-- | Try to parse the given text as a symbol
pSymbol :: Text -> Parser Text
pSymbol = Lexer.symbol pSpace

-- | Characters we don't care about are spaces and newlines. Should probably add clrf etc also.
pSpace :: Parser ()
pSpace = do
    many $ choice [spaceChar, newline]
    return ()