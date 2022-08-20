{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ExplicitForAll     #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
module LexerExample where

-- megaparsec
import Text.Megaparsec hiding (State, Pos, SourcePos)
import Text.Megaparsec.Char

-- parser-combinators
import Control.Monad.Combinators.Expr
    ( makeExprParser, Operator(Postfix, InfixL, Prefix) )

import Control.Applicative hiding (some, many)
import Control.Monad (void)

import Data.Text (Text)
import qualified Data.Text as T
import Data.Foldable (traverse_)
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Void

------------------------------------------------------------

type Parser = Parsec Void Text

-- augments the given parser by consuming all trailing whitespace
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- matches the given text and consumes all trailing whitespace
symbol :: Text -> Parser Text
symbol = L.symbol sc

------------------------------------------------------------

charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') L.charLiteral

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

------------------------------------------------------------

integer :: Parser Integer
integer = lexeme L.decimal

float :: Parser Double
float = lexeme L.float

signedInteger :: Parser Integer
signedInteger = L.signed (return ()) integer

signedFloat :: Parser Double
signedFloat = L.signed (return ()) float

------------------------------------------------------------

data Expr
  = ExprVar String
  | ExprInt Int
  | ExprNeg Expr
  | ExprSum Expr Expr
  | ExprSub Expr Expr
  | ExprMul Expr Expr
  | ExprDiv Expr Expr
  deriving (Eq, Ord, Show)

pVariable :: Parser Expr
pVariable = ExprVar <$> lexeme ((((:) <$> letterChar) <*> many alphaNumChar) <?> "variable")

pInteger :: Parser Expr
pInteger = ExprInt <$> lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pTerm :: Parser Expr
pTerm = choice
  [ parens pExpr
  , pVariable
  , pInteger ]

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

------------------------------------------------------------

-- data Operator m a
--   = InfixN  (m (a -> a -> a)) -- ^ Non-associative infix
--   | InfixL  (m (a -> a -> a)) -- ^ Left-associative infix
--   | InfixR  (m (a -> a -> a)) -- ^ Right-associative infix
--   | Prefix  (m (a -> a))      -- ^ Prefix
--   | Postfix (m (a -> a))      -- ^ Postfix

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ prefix "-" ExprNeg
    , prefix "+" id      ]
  , [ binary "*" ExprMul
    , binary "/" ExprDiv ]
  , [ binary "+" ExprSum
    , binary "-" ExprSub ]
  ]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary  name f = InfixL  (f <$ symbol name)

prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix  name f = Prefix  (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

------------------------------------------------------------

-- nonIndented :: (TraversableStream s, MonadParsec e s m)
--   => m ()              -- ^ How to consume indentation (white space)
--   -> m a               -- ^ Inner parser
--   -> m a

-- indentBlock :: (TraversableStream s, MonadParsec e s m, Token s ~ Char)
--   => m ()              -- ^ How to consume indentation (white space ; must include newlines)
--   -> m (IndentOpt m a b) -- ^ How to parse “reference” token
--   -> m a

-- data IndentOpt m a b
--   = IndentNone a
--     -- ^ Parse no indented tokens, just return the value
--   | IndentMany (Maybe Pos) ([b] -> m a) (m b)
--     -- ^ Parse many indented tokens (possibly zero), use given indentation
--     -- level (if 'Nothing', use level of the first indented token); the
--     -- second argument tells how to get the final result, and the third
--     -- argument describes how to parse an indented token
--   | IndentSome (Maybe Pos) ([b] -> m a) (m b)
--     -- ^ Just like 'IndentMany', but requires at least one indented token to
--     -- be present

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

scn :: Parser ()
scn = L.space space1 lineComment empty

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment empty

-- header and list items
pItemList :: Parser (String, [String])
pItemList = L.nonIndented scn (L.indentBlock scn p)
  where
    p = do
      header <- pItem
      return (L.IndentMany Nothing (return . (header, )) pItem)

pItem :: Parser String
pItem = lexeme (some (alphaNumChar <|> char '-')) <?> "list item"

------------------------------------------------------------

example :: Text
example = "head\n  item1\n  item2\n item3"

main :: IO ()
main = do
  putStrLn "hello, world!"
  parseTest (pItemList <* eof) example