{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Trace where

import Data.Text
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Control.DeepSeq
import GHC.Generics
import Data.Int
import Data.Word

import Core hiding (Result, Fork)

type Parser a = Parsec Void Text a

data OutputEntry = Instant Word64 Int      -- ^ now, size of eventqueue
                 | Event Word64 SSMExp     -- ^ now, new variable value
                 | Fork [String]        -- ^ Fork call, names of forked procedures
                 | Result String SSMExp -- ^ variable name and final value
                 | Crash
                 | EventQueueFull
                 | NegativeDepth
                 | BadAfter
  deriving (Show, Eq, Generic, NFData)

type Output = [OutputEntry]

testoutput = Prelude.unlines [ "event 0 value int 0"
                             , "event 1 value bool 1"
                             , "negative depth"
                             , "result a uint64 2343546543245"
                             , "event 2 value int (-1)"
                             , "now 3 eventqueuesize 3"
                             , "bad after"
                             , "now 5 eventqueuesize 5"
                             , "fork mywait mywait mysum"
                             , "event 6 value int 7"
                             , "result x int 1"
                             , "fork mywait"
                             , "bad after"
                             , "negative depth"
                             , "eventqueue full"
                             , "result y bool 0"
                             , "bad after"
                             , "result zt uint64 3423523234"
                             , "result z bool 1"
                             , "negative depth"
                             , "eventqueue full"
                             , "bad after"
                             ]

mkTrace :: String -> Output
mkTrace inp = fromRight $ parse pTrace "" (pack inp)
  where
      fromRight :: Show a => Either a b -> b
      fromRight (Left a)  = error $ "parsing of test results failed: " ++ show a
      fromRight (Right b) = b

parseLine :: String -> Maybe OutputEntry
parseLine inp = toMaybe $ parse pTraceItem "" (pack inp)
  where
      toMaybe :: Show a => Either a b -> Maybe b
      toMaybe (Left a)  = Nothing
      toMaybe (Right b) = Just b

pTrace :: Parser Output
pTrace = many pTraceItem

pTraceItem :: Parser OutputEntry
pTraceItem = choice [try pEvent, try pInstant, pBadAfter, pResult, pFork, pCrash, pEventQueueFull, pNegativeDepth]

pFork :: Parser OutputEntry
pFork = do
    pSpace
    pSymbol "fork"
    pSpace
    procs <- some (try pIdent)
    pSpace
    return $ Fork procs

pCrash :: Parser OutputEntry
pCrash = do
    pSpace
    pSymbol "crash"
    pSpace
    return Crash

pBadAfter :: Parser OutputEntry
pBadAfter = do
    pSpace
    pSymbol "bad"
    pSpace
    pSymbol "after"
    return BadAfter

pNegativeDepth :: Parser OutputEntry
pNegativeDepth = do
    pSpace
    pSymbol "negative"
    pSpace
    pSymbol "depth"
    return NegativeDepth

pEventQueueFull :: Parser OutputEntry
pEventQueueFull = do
    pSpace
    pSymbol "eventqueue"
    pSpace
    pSymbol "full"
    return EventQueueFull

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
                 , "crash"
                 , "now"
                 , "result"
                 , "bad"
                 , "after"
                 , "eventqueuesize"
                 , "eventqueue"
                 , "full"
                 , "negative"
                 , "depth"
                 , "int"
                 , "int64"
                 , "uint64"
                 , "bool"]
pRes :: Parser SSMExp
pRes = do
    -- add more variants here as we add more types
    choice [pInt64, pUInt64, pInt, pBool]
  where
      pInt :: Parser SSMExp
      pInt = do
          pSpace
          pSymbol "int"
          pSpace
          num <- choice [try (parens signed), signed]
          pSpace
          return $ Lit TInt32 . LInt32 $ num

      pInt64 :: Parser SSMExp
      pInt64 = do
          pSpace
          pSymbol "int64"
          pSpace
          num <- choice [ try (parens signed), signed]
          pSpace
          return $ Lit TInt64 $ LInt64 $ num

      pUInt64 :: Parser SSMExp
      pUInt64 = do
          pSpace
          pSymbol "uint64"
          pSpace
          num <- choice [ try (parens signed), Lexer.decimal]
          pSpace
          return $ Lit TUInt64 $ LUInt64 num

      pBool :: Parser SSMExp
      pBool = do
          pSpace
          pSymbol "bool"
          pSpace
          b <- Lexer.lexeme pSpace Lexer.decimal
          case b of
              0 -> return $ Lit TBool $ LBool False
              _ -> return $ Lit TBool $ LBool True

      parens :: Parser a -> Parser a
      parens p = do
          char '('
          res <- p
          char ')'
          return res

      signed :: (Integral a, Num a) => Parser a
      signed = Lexer.signed pSpace Lexer.decimal

pEventqueuesize :: Parser Int
pEventqueuesize = do
    pSpace
    pSymbol "eventqueuesize"
    pSpace
    num <- Lexer.lexeme pSpace Lexer.decimal
    pSpace
    return num

pNow :: Parser Word64
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
