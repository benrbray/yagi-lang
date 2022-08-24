{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Yagi.ParserNew (
  Expr(..), Ident(..), Abstraction(..),
  variable, universe,
  lambdaAbstraction,
  piAbstraction,
  expr,
  parse, ParseResult(..),
  lineColFromOffset, offsetFromLineCol,
  termAtPos
) where

-- misc
import Data.Void
import Data.Tuple.Extra

-- text
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- structures
import Control.Monad (void, ap, when)
import Data.Foldable (traverse_)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))

-- megaparsec, parser-combinators
import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators (many, some)

-- yagi-lang
import TextSpan (TextSpan(..))
import qualified TextSpan as TS
import Yagi.Syntax

------------------------------------------------------------

type Parser = MP.Parsec Void TextSpan

---- symbols -----------------------------------------------

spanOnly :: Parser TextSpan -> Parser OffsetSpan
spanOnly ts = getSpan <$> ts

_fun :: Parser OffsetSpan
_fun = spanOnly $ TS.symbol "fun"

_forall :: Parser OffsetSpan
_forall = spanOnly $ TS.symbol "forall"

_colon :: Parser OffsetSpan
_colon = spanOnly $ TS.symbol ":"

_colonEqual :: Parser OffsetSpan
_colonEqual = spanOnly $ TS.symbol ":="

_comma :: Parser OffsetSpan
_comma = spanOnly $ TS.symbol ","

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
  [ MP.try spine
  , lambdaAbstraction
  , piAbstraction
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

piAbstraction :: Parser (Expr OffsetSpan)
piAbstraction = do
  (Span a _) <- _forall
  (sp, bgs) <- bindGroups
  _ <- _comma
  (Span _ b, context) <- withSpan <$> expr
  return $ mkLambda (Span a b) (Bindings (map convert bgs)) context
  where 
    convert :: BindMany -> ([Ident], Expr OffsetSpan)
    convert (BindMany names tpe) = (map snd names, tpe)

------------------------------------------------------------

offsetFromLineCol :: ParseResult -> PosLineCol -> PosOffset
offsetFromLineCol ParseResult{..} = offsetFromLineCol' parsedLines

-- TODO make efficient with binary tree
offsetFromLineCol' :: [PosOffset] -> PosLineCol -> PosOffset
offsetFromLineCol' _  (PosLineCol 0 c) = PosOffset c
offsetFromLineCol' ls (PosLineCol l c) = PosOffset $ sum (take l offs) + c
  where offs = map posOffset ls

-- TODO make efficient with binary tree
lineColFromOffset
  :: ParseResult -- line lengths
  -> PosOffset   -- absolute offset into the string
  -> PosLineCol
lineColFromOffset ParseResult{..} (PosOffset off) = go 0 0 (map posOffset parsedLines)
  where go
          :: Int   -- accumulator (lines consumed so far)
          -> Int   -- accumulator (sum of line lengths so far)
          -> [Int] -- rest
          -> PosLineCol
        go l acc [] = PosLineCol l (off - acc)  -- should not happen
        go l acc (x:xs)
          | off < acc + x = PosLineCol l (off - acc)
          | otherwise     = go (l+1) (acc + x) xs

data ParseResult = ParseResult
  { parsedExpr  :: Expr OffsetSpan
  , parsedLines :: [PosOffset]
  } deriving (Show, Eq)

parse :: Text -> Either Text ParseResult
parse t =
  case result of
    Left peb -> Left . T.pack . show $ MP.errorBundlePretty peb
    Right x -> Right $ ParseResult x (PosOffset <$> lines)
  where result = MP.runParser expr "[filename]" (TS.textSpan t)
        lines = map ( (1+) . T.length ) $ T.lines t