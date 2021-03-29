{-# LANGUAGE OverloadedStrings #-}
module Trace where

import Data.Text
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser a = Parsec Void Text a

data OutputEntry = Instant Int Int
                 | Event Int String
                 | Result String String
  deriving (Show, Eq)

type Output = [OutputEntry]

mkTrace :: String -> Output
mkTrace inp = fromRight $ parse pTrace "" (pack inp)
  where
      fromRight :: Show a => Either a b -> b
      fromRight (Left a)  = error $ "parsing of test results failed: " ++ show a
      fromRight (Right b) = b

pTrace :: Parser Output
pTrace = many $ choice [pInstant, pResult, pEvent]

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

pRes :: Parser String
pRes = do
    -- add more variants here as we add more types
    choice [pInt, pBool]
  where
      pInt :: Parser String
      pInt = show <$> Lexer.lexeme pSpace Lexer.decimal

      pBool :: Parser String
      pBool = choice [unpack <$> pSymbol "true", unpack <$> pSymbol "false"]

pEventqueuesize :: Parser Int
pEventqueuesize = do
    pSpace
    pSymbol "eventqueuesize"
    pSpace
    char ':'
    pSpace
    num <- Lexer.lexeme pSpace Lexer.decimal
    pSpace
    return num

pNow :: Parser Int
pNow = do
    pSymbol "now"
    pSpace
    char ':'
    pSpace
    num <- Lexer.lexeme pSpace Lexer.decimal 
    pSpace
    return num

pSymbol :: Text -> Parser Text
pSymbol = Lexer.symbol pSpace

pSpace :: Parser ()
pSpace = do
    many $ choice [spaceChar, newline]
    return ()