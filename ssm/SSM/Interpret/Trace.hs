{-| This module declares a @Trace@ datatype that is used to describe output of a SSM
program. The interpreter directly outputs elements of this type, while the generated
C code prints lines to the terminal representing the trace items. For this reason
there is also a parser for trace items implemented by this module.

This module and the data type it exposes are tightly coupled to the testing framework.
The trace items are used to compare the C code and the interpreter for equality, making
sure that they have the same semantics. The interpreter should not necessarily be so
tightly coupled with the testing framework, so this module and the interpreter should
eventually be refactored to reflect this. -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module SSM.Interpret.Trace
    ( -- ** Trace items
      -- | The actual trace items that can be emitted by the interpreter/RTS
      OutputEntry(..)
      -- ** Interpreter output
      -- | The interpreter pretty much just outputs a list of trace items
    , Output(..)

      -- ** Parser
      {- | To handle the output produced by the generated C code, this parser is
      available. It will parse one line of output at a time. -}
    , parseLine
    ) where

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

import SSM.Core.Syntax hiding (Fork)

import System.IO.Unsafe

trace :: Show a => a -> a
trace x = unsafePerformIO $ putStrLn (show x) >> return x

-- | Textbook parser definition
type Parser a = Parsec Void T.Text a

-- | Variants of output that evaluating a SSM programs can produce
data OutputEntry
    {-| @Instant n s@ says that model time @n@ just finished evaluating, and that
    there are @s@ events in the event queue currently. -}
    = Instant Word64 Int
    {-| @Event n v@ says that we just performed an update that was scheduled for model
    time @n@ by giving a variable the new value @v@. Ideally we'll alter this to also
    include the variable name, for greater clarity. -}
    | Event Word64 SSMExp
    {-| @Fork [procs]@ says that we are about to fork the processes identified by their
    names in @procs@. -}
    | Fork [String]
    {- | @Result var v@ says that after the program terminated, the variable @var@ had
    the final value @v@. -}
    | Result String SSMExp
    -- | @NumConts s@ says that the size of the readyqueue is currently @s@.
    | NumConts Int
    -- | @Crash@ says that the program crashed.
    | Crash
    {- | @EventQueueFull@ says that the program crashed because the event queue ran out
    of space. -}
    | EventQueueFull
    {- | @ContQueueFull@ says that the program crashed because the ready queue ran out of
    space. -}
    | ContQueueFull
    {- | @NegativeDepth@ says that the program crashed because the depth of a process ran
    out as it was forking more children. -}
    | NegativeDepth
    {- | @BadAfter@ says that the program crashed because an attempt was made to schedule
    an update for a variable at a time that had already passed. -}
    | BadAfter
    deriving (Show, Eq)

-- | The interpreter output is simply a list of `OutputEntry`
type Output = [OutputEntry]

testoutput = Prelude.unlines [ "event 0 value i32 0"
                             , "event 1 value bool 1"
                             , "negative depth"
                             , "contqueue full"
                             , "result a u64 2343546543245"
                             , "event 2 value i32 (-1)"
                             , "now 3 eventqueuesize 3"
                             , "bad after"
                             , "contqueue full"
                             , "numconts 283"
                             , "now 5 eventqueuesize 5"
                             , "fork mywait mywait mysum"
                             , "event 6 value i32 7"
                             , "result x i64 1"
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
                             , "result zt u64 3423523234"
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

-- | Parse a single line of output from the C code
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
                 , "i32"
                 , "i64"
                 , "u64"
                 , "bool"]
pRes :: Parser SSMExp
pRes = do
    -- add more variants here as we add more types
    choice [pInt64, pUInt64, pInt, pBool]
  where
      pInt :: Parser SSMExp
      pInt = do
          pSymbol "i32"
          num <- choice [try (parens signed), signed]
          return $ Lit TInt32 $ LInt32 num

      pInt64 :: Parser SSMExp
      pInt64 = do
          pSymbol "i64"
          num <- choice [ try (parens signed), signed]
          return $ Lit TInt64 $ LInt64 num

      pUInt64 :: Parser SSMExp
      pUInt64 = do
          pSymbol "u64"
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
