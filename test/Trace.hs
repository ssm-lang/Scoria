{-# LANGUAGE OverloadedStrings #-}
module Trace where

import Data.Text
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Text.Megaparsec.Debug

import Control.Monad

type Parser a = Parsec Void Text a

data Trace = Trace { instants :: [Instant]
                   , result :: [Result]
                   }
  deriving (Show, Eq)

data Instant = Instant Int Int deriving (Show, Eq)
data Result = Result String String deriving (Show, Eq)

mkTrace :: String -> Trace
mkTrace inp = fromRight $ parse pTrace "" (pack inp)
  where
      fromRight :: Show a => Either a b -> b
      fromRight (Left a) = error $ "parsing of test results failed: " ++ show a
      fromRight (Right b) = b

pTrace :: Parser Trace
pTrace = do
    instants <- some pInstant
    results <- many pResult
    return $ Trace instants results

pInstant :: Parser Instant
pInstant = do
    now <- pNow
    eventqueuesize <- pEventqueuesize
    return $ Instant now eventqueuesize

pResult :: Parser Result
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