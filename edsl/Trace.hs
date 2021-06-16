{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Trace where

import qualified Data.Text as T
import Data.Void
import Data.List

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Control.DeepSeq
import GHC.Generics
import Data.Int
import Data.Word

import Core hiding (Result, Fork)

import System.IO.Unsafe

trace :: Show a => a -> a
trace x = unsafePerformIO $ putStrLn (show x) >> return x

type Parser a = Parsec Void T.Text a

data OutputEntry = Instant Word64 Int      -- ^ now, size of eventqueue
                 | Event Word64 SSMExp     -- ^ now, new variable value
                 | Fork [String]        -- ^ Fork call, names of forked procedures
                 | Result String SSMExp -- ^ variable name and final value
                 | NumConts Int
                 | Crash
                 | EventQueueFull
                 | ContQueueFull
                 | NegativeDepth
                 | BadAfter
  deriving (Show, Eq, Generic, NFData)

type Output = [OutputEntry]

{-
Events occur between instants, and all that can happen when events are applied
between instants is that processes are enqueued in the ready queue. Since this is
a priority queue it does not matter which order the events are applied in, as long as
the same events are applied in the same instant.
-}
instance {-# OVERLAPPING #-} Eq Output where
    xs == ys = equalSemantics xs ys

equalSemantics :: Output -> Output -> Bool
equalSemantics xs ys = case (xs,ys) of
    ([],[]) -> True

    (xs@(Event _ _:_), ys@(Event _ _:_)) ->
        let (xevents, xs') = takeEvents xs
            (yevents, ys') = takeEvents ys

        in equalEvents xevents yevents && equalSemantics xs' ys'

    ((x:xs), (y:ys)) -> x == y && equalSemantics xs ys

    (_,_)            -> False

  where
      takeEvents :: Output -> (Output, Output)
      takeEvents xs = takeEvents_ ([], xs)

      takeEvents_ :: (Output, Output) -> (Output, Output)
      takeEvents_ (xs, [])     = ([], [])
      takeEvents_ (xs, (y:ys)) =
          if isEvent y
              then takeEvents_ (y : xs, ys)
              else if isCrash y
                       then ([y], ys)
                       else (xs, y:ys)

      -- | When we've gathered all events that were applied we have three scenarios.
      -- 1: There was not enough time for the interpreter and c-code generator to
      --    generate enough trace to finish applying the events. In this case we might
      --    have a case where the invariant is broken, so we'll return True since we're
      --    between instants (where the invariant is allowed to eb broken).
      -- 2: One of them crashed while the other did not have time to finish applying all
      --    of its events. In this case we return True after verifying that a crash
      --    occured.
      -- 3: Fetching the events went alright, and there were enough time for them to
      --    finish applying all their events. In that case we'll just make sure that the
      --    events are the same, but not that they are necessarily applied in the same order.
      equalEvents :: Output -> Output -> Bool
      equalEvents [] []  = True
      equalEvents [x] [] = isCrash x
      equalEvents [] [x] = isCrash x
      equalEvents xs ys  =
          let eventunion     = union xs ys
              lengthxevents  = length xs
              lengthyevents  = length ys
              lengthunion    = length eventunion
          in lengthunion   == lengthxevents &&
             lengthxevents == lengthyevents

      -- | Returns True if the entry is an event
      isEvent :: OutputEntry -> Bool
      isEvent (Event _ _) = True
      isEvent _           = False

      -- | Returns True if a crash happened
      isCrash :: OutputEntry -> Bool
      isCrash Crash          = True
      isCrash EventQueueFull = True
      isCrash ContQueueFull  = True
      isCrash NegativeDepth  = True
      isCrash BadAfter       = True
      isCrash _              = False

testoutput = Prelude.unlines [ "event 0 value int32 0"
                             , "event 1 value bool 1"
                             , "negative depth"
                             , "contqueue full"
                             , "result a uint64 2343546543245"
                             , "event 2 value int32 (-1)"
                             , "now 3 eventqueuesize 3"
                             , "bad after"
                             , "contqueue full"
                             , "numconts 283"
                             , "now 5 eventqueuesize 5"
                             , "fork mywait mywait mysum"
                             , "event 6 value int32 7"
                             , "result x int64 1"
                             , "fork mywait"
                             , "numconts 325"
                             , "contqueue full"
                             , "bad after"
                             , "negative depth"
                             , "eventqueue full"
                             , "result y bool 0"
                             , "bad after"
                             , "numconts 0"
                             , "contqueue full"
                             , "result zt uint64 3423523234"
                             , "result z bool 1"
                             , "numconts 28387"
                             , "negative depth"
                             , "eventqueue full"
                             , "contqueue full"
                             , "bad after"
                             , "numconts 12345"
                             ]

mkTrace :: String -> Output
mkTrace inp = fromRight $ parse pTrace "" (T.pack inp)
  where
      fromRight :: Show a => Either a b -> b
      fromRight (Left a)  = error $ "parsing of test results failed: " ++ show a
      fromRight (Right b) = b

parseLine :: String -> Maybe OutputEntry
parseLine inp = toMaybe $ parse pTraceItem "" (T.pack inp)
  where
      toMaybe :: Show a => Either a b -> Maybe b
      toMaybe (Left a)  = Nothing
      toMaybe (Right b) = Just b

pTrace :: Parser Output
pTrace = many (pSpace *> pTraceItem)

pTraceItem :: Parser OutputEntry
pTraceItem = choice [ try pEvent
                    , try pInstant
                    , pBadAfter
                    , pResult
                    , pFork
                    , pNumConts
                    , pCrash
                    , pEventQueueFull
                    , pContQueueFull
                    , pNegativeDepth
                    ]

pFork :: Parser OutputEntry
pFork = do
    pSymbol "fork"
    procs <- some (try pIdent)
    return $ Fork procs

pNumConts :: Parser OutputEntry
pNumConts = do
    pSymbol "numconts"
    num <- Lexer.lexeme pSpace Lexer.decimal
    return $ NumConts num

pCrash :: Parser OutputEntry
pCrash = do
    pSymbol "crash"
    return Crash

pBadAfter :: Parser OutputEntry
pBadAfter = do
    pSymbol "bad"
    pSymbol "after"
    return BadAfter

pNegativeDepth :: Parser OutputEntry
pNegativeDepth = do
    pSymbol "negative"
    pSymbol "depth"
    return NegativeDepth

pEventQueueFull :: Parser OutputEntry
pEventQueueFull = do
    pSymbol "eventqueue"
    pSymbol "full"
    return EventQueueFull

pContQueueFull :: Parser OutputEntry
pContQueueFull = do
    pSymbol "contqueue"
    pSymbol "full"
    return ContQueueFull

pEvent :: Parser OutputEntry
pEvent = do
    pSymbol "event"
    num <- Lexer.lexeme pSpace Lexer.decimal
    pSymbol "value"
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
    refname <- pIdent
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
                 , "contqueue"
                 , "numconts"
                 , "full"
                 , "negative"
                 , "depth"
                 , "int32"
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
          pSymbol "int32"
          num <- choice [try (parens signed), signed]
          return $ Lit TInt32 . LInt32 $ num

      pInt64 :: Parser SSMExp
      pInt64 = do
          pSymbol "int64"
          num <- choice [ try (parens signed), signed]
          return $ Lit TInt64 $ LInt64 $ num

      pUInt64 :: Parser SSMExp
      pUInt64 = do
          pSymbol "uint64"
          num <- choice [ try (parens signed), Lexer.decimal]
          return $ Lit TUInt64 $ LUInt64 num

      pBool :: Parser SSMExp
      pBool = do
          pSymbol "bool"
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
    pSymbol "eventqueuesize"
    num <- Lexer.lexeme pSpace Lexer.decimal
    return num

pNow :: Parser Word64
pNow = do
    pSymbol "now"
    num <- Lexer.lexeme pSpace Lexer.decimal 
    return num

-- | Try to parse the given text as a symbol
pSymbol :: T.Text -> Parser T.Text
pSymbol = Lexer.symbol pSpace

-- | Characters we don't care about are spaces and newlines. Should probably add clrf etc also.
pSpace :: Parser ()
pSpace = do
    many $ choice [spaceChar, newline]
    return ()
