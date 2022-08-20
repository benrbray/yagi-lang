{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ExplicitForAll     #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
module ExprF where

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

data ExprF a
  = FVar String
  | FInt Int
  | FNeg a
  | FSum a a
  | FSub a a
  | FMul a a
  | FDiv a a
  deriving (Eq, Show, Functor)

data Span
  = Span Int Int
  deriving (Eq,Show)

data WithSpan a
  = WithSpan Span a
  deriving (Eq,Show,Functor)

newtype Mu f = InF { outF :: f (Mu f) }

newtype ExprWithSpanF a
  = ExprWithSpanF (WithSpan (ExprF a))
  deriving (Eq, Show, Functor)

type ExprWithSpan = Mu ExprWithSpanF

exprWithSpan :: Span -> ExprF (Mu ExprWithSpanF) -> ExprWithSpan
exprWithSpan s e = InF $ ExprWithSpanF (WithSpan s e)

x :: ExprWithSpan
x = exprWithSpan (Span 0 0) (FVar "")

------------------------------------------------------------

type Parser = Parsec Void Text

-- augments the given parser by consuming all trailing whitespace
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- matches the given text and consumes all trailing whitespace
symbol :: Text -> Parser Text
symbol = L.symbol sc

sc,scn :: Parser ()
scn = L.space space1 empty empty
sc = L.space (void $ some (char ' ' <|> char '\t')) empty empty