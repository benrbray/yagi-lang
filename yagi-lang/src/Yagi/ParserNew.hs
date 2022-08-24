{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Yagi.ParserNew (
  Expr(..), Ident(..), Abstraction(..),
  variable, universe,
  lambdaAbstraction
) where

-- misc
import System.IO
import Data.Void
import Data.Proxy
import Data.Tuple.Extra

-- text
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- structures
import Control.Applicative hiding (some, many)
import Control.Monad (void, ap, when)
import Data.Functor
import Data.Foldable (traverse_)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))

-- megaparsec, parser-combinators
import Text.Megaparsec.Char
import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators (many, some)

-- yagi-lang
import TextSpan (TextSpan(..))
import qualified TextSpan as TS
import Yagi.RecursionSchemes
import Util.PrettyPrint
import Util.Tuple
import Yagi.Syntax

------------------------------------------------------------

type Parser = MP.Parsec Void TextSpan

---- symbols -----------------------------------------------

spanOnly :: Parser TextSpan -> Parser OffsetSpan
spanOnly ts = getSpan <$> ts

_fun :: Parser OffsetSpan
_fun = spanOnly $ TS.symbol "fun"

_colon :: Parser OffsetSpan
_colon = spanOnly $ TS.symbol ":"

_colonEqual :: Parser OffsetSpan
_colonEqual = spanOnly $ TS.symbol ":="

_arrow :: Parser OffsetSpan
_arrow = spanOnly $ TS.symbol "->"

_leftparen :: Parser OffsetSpan
_leftparen = spanOnly $ TS.symbol "("

_rightparen :: Parser OffsetSpan
_rightparen = spanOnly $ TS.symbol ")"

---- combinators -------------------------------------------

parens :: Parser a -> Parser a
parens = MP.between (TS.symbol "(") (TS.symbol ")")

many1 :: Parser a -> Parser (NonEmpty a)
many1 p = (:|) <$> p <*> many p

------------------------------------------------------------

ident :: Parser (OffsetSpan, Ident)
ident = TS.lexeme name >>= check
  where
    name :: Parser TextSpan
    name = TS.cons <$> MP.satisfy TS.isAlpha <*> MP.takeWhileP Nothing TS.isAlphaNum
    reserved = ["fun", "forall", "Type"]
    check :: TextSpan -> Parser (OffsetSpan, Ident)
    check ts@TextSpan{..} =
      if tsText `elem` reserved
      then fail $ "keyword " ++ show tsText ++ " cannot be used as a variable name"
      else return (getSpan ts, NamedIdent tsText)

variable :: Parser (Expr OffsetSpan)
variable = uncurry mkVar <$> ident

---- expressions -------------------------------------------

spine :: Parser (Expr OffsetSpan)
spine = foldl1 wrap <$> some simpleExpr
  where
    wrap :: Expr OffsetSpan -> Expr OffsetSpan -> Expr OffsetSpan
    wrap a b = mkApp (Span l r) a b
      where Span l _ = getSpan a
            Span _ r = getSpan b

expr :: Parser (Expr OffsetSpan)
expr = MP.choice
  [ spine
  ]

simpleExpr :: Parser (Expr OffsetSpan)
simpleExpr = MP.choice
  [ variable
  , universe
  , parens expr
  ]

universe :: Parser (Expr OffsetSpan)
universe = wrap <$> (TS.symbol "Type" *> TS.lexeme TS.decimal <?> "universe")
  where
    wrap :: (TextSpan, Int) -> Expr OffsetSpan
    wrap (ts, k) = mkExpr $ Universe (getSpan ts) k

---- function syntax ---------------------------------------

-- intermediate type used when parsing function abstractions
-- TODO can we just use `Bindings` type directly?
data BindMany = BindMany
  { bmNames :: [(OffsetSpan, Ident)]
  , bmType  :: Expr OffsetSpan
  }

bindGroup :: Parser (OffsetSpan, BindMany)
bindGroup = do
  (Span a _) <- _leftparen
  ids <- some ident
  _   <- _colon
  e   <- expr
  (Span _ b) <- _rightparen
  return (Span a b, BindMany ids e)

bindGroups :: Parser (OffsetSpan, [BindMany])
bindGroups = do
  (spans, binds) <- unzip <$> some bindGroup
  let Span a _ = head spans
      Span _ b = last spans
  return (Span a b, binds)

lambdaAbstraction :: Parser (Expr OffsetSpan)
lambdaAbstraction = do
  (Span a _) <- _fun
  (sp, bgs) <- bindGroups
  _ <- _arrow
  (Span _ b, context) <- withSpan <$> expr
  return $ mkLambda (Span a b) (Bindings (map convert bgs)) context
  where 
    convert :: BindMany -> ([Ident], Expr OffsetSpan)
    convert (BindMany names tpe) = (map snd names, tpe)