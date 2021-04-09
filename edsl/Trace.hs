{-# LANGUAGE OverloadedStrings #-}
module Trace where

import Data.Text
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

import Core hiding (Result, Fork)

type Parser a = Parsec Void Text a

data OutputEntry = Instant Int Int      -- ^ now, size of eventqueue
                 | Event Int SSMExp     -- ^ now, new variable value
                 | Fork [String]        -- ^ Fork call, names of forked procedures
                 | Result String SSMExp -- ^ variable name and final value
  deriving (Show, Eq)

type Output = [OutputEntry]

testoutput = Prelude.unlines [ "event 0 value int 0"
                             , "event 1 value bool 1"
                             , "event 2 value int (-1)"
                             , "now 3 eventqueuesize 3"
                             , "now 5 eventqueuesize 5"
                             , "fork mywait mywait mysum"
                             , "event 6 value int 7"
                             , "result x int 1"
                             , "fork mywait"
                             , "result y bool 0"
                             , "result z bool 1"
                             ]

mkTrace :: String -> Output
mkTrace inp = fromRight $ parse pTrace "" (pack inp)
  where
      fromRight :: Show a => Either a b -> b
      fromRight (Left a)  = error $ "parsing of test results failed: " ++ show a
      fromRight (Right b) = b

parseLine :: String -> Maybe OutputEntry
parseLine inp = toMaybe $ parse (choice [pEvent, pInstant, pResult, pFork]) "" (pack inp)
  where
      toMaybe :: Show a => Either a b -> Maybe b
      toMaybe (Left a)  = Nothing
      toMaybe (Right b) = Just b

pTrace :: Parser Output
pTrace = many $ choice [pEvent, pInstant, pResult, pFork]

pFork :: Parser OutputEntry
pFork = do
    pSpace
    pSymbol "fork"
    pSpace
    procs <- some (try pIdent)
    pSpace
    return $ Fork procs

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
    let res = a:as
    if res `elem` keywords
        then fail "found keyword, expected identifier"
        else return res
  where
      keywords :: [String]
      keywords = [ "event"
                 , "fork"
                 , "now"
                 , "result"
                 , "eventqueuesize"
                 , "int"
                 , "bool"]
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
          num <- choice [try (parens signed), signed]
          pSpace
          return $ Lit TInt . LInt $ num
        where
            parens :: Parser a -> Parser a
            parens p = do
                char '('
                res <- p
                char ')'
                return res

            signed :: Parser Int
            signed = Lexer.signed pSpace Lexer.decimal

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