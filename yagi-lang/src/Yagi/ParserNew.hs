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

------------------------------------------------------------

data Ident
  = NamedIdent Text     -- variables named in the source code
  | SynthIdent Text Int -- for variable substitution
  | Dummy               -- used only for pretty printing
  deriving (Show, Eq, Ord)

newtype Bindings a
  = Bindings [([Ident],a)]
  deriving (Show, Eq, Functor)

data Abstraction (a :: *) = Abstraction
  { absBindings :: Bindings a  -- (identifier, type)
  , absExpr     :: a           -- binding context
  } deriving (Show, Eq, Functor)

------------------------------------------------------------

data ExprF (s :: *) (a :: *)
  = Var s Ident
  | Universe s Int
  | Pi s (Abstraction a)
  | Lambda s (Abstraction a)
  | App s a a
  deriving (Show, Eq, Functor)

type Expr s = Mu (ExprF s)

mkExpr :: ExprF s (Expr s) -> Expr s
mkExpr = InF

mkVar :: s -> Ident -> Expr s
mkVar a b = InF $ Var a b

mkUniverse :: s -> Int -> Expr s
mkUniverse a b = InF $ Universe a b

mkPi :: s -> Bindings (Expr s) -> Expr s -> Expr s
mkPi a b c = InF $ Pi a (Abstraction b c)

mkLambda :: s -> Bindings (Expr s) -> Expr s -> Expr s
mkLambda a b c = InF $ Lambda a (Abstraction b c)

mkApp :: s -> Expr s -> Expr s -> Expr s
mkApp a b c = InF $ App a b c

------------------------------------------------------------

instance PrettyPrint Ident where
  pretty (NamedIdent t)   = t
  pretty (SynthIdent t i) = T.concat [t, "@", T.pack $ show i]
  pretty Dummy            = "*"

instance (PrettyPrint a) => PrettyPrint (Bindings a) where
  pretty (Bindings bs)
    = T.concat
      [ "{", T.intercalate ", " (map go bs), "}" 
      ]
    where go :: ([Ident], a) -> Text
          go (ids,e) = T.concat [ "(", T.intercalate " " $ map pretty ids, ":", pretty e, ")" ]

-- show a single layer
instance (Show p, PrettyPrint a) => PrettyPrint (ExprF (Span p) a) where
  pretty (Var s x) = pretty x
  pretty (Universe s u) = T.concat ["Type", T.pack $ show u]
  pretty (Pi s (Abstraction bindings expr)) 
    = T.concat
      [ "(Π "  , pretty bindings
      , " -> " , pretty expr     , ")" ] 
  pretty (Lambda s (Abstraction bindings expr))
    = T.concat
        [ "(λ " , pretty bindings
        , ". "  , pretty expr     , ")" ]
  pretty (App s e1 e2)
    = T.concat
        [ pretty e1, " (", pretty e2, ")"]

-- show an entire expression
instance Show p => Show (Mu (ExprF (Span p))) where
  show (InF expr) = show expr

-- pretty-print an entire expression
instance Show p => PrettyPrint (Mu (ExprF (Span p))) where
  pretty (InF expr) = pretty expr

------------------------------------------------------------

newtype PosOffset = PosOffset Int deriving (Show, Eq)

data PosLineCol = PosLineCol
  { posLine :: !Int
  , posCol  :: !Int
  } deriving (Show, Eq, Ord)

data Span p = Span
  { spanStart :: p
  , spanEnd   :: p
  } deriving (Show, Eq, Ord)

type OffsetSpan = Span PosOffset
type LineColSpan = Span PosLineCol

emptyLineColSpan :: Span PosLineCol
emptyLineColSpan = Span (PosLineCol 0 0) (PosLineCol 0 0)

emptyOffsetSpan :: Span PosOffset
emptyOffsetSpan = Span (PosOffset 0) (PosOffset 0)

------------------------------------------------------------

class HasSpan p m where
  getSpan :: m -> Span p
  withSpan :: m -> (Span p, m)
  withSpan = toFst getSpan

instance HasSpan p (Expr (Span p)) where
  getSpan (InF (Var s _)) = s
  getSpan (InF (Universe s _)) = s
  getSpan (InF (Pi s _)) = s
  getSpan (InF (Lambda s _)) = s
  getSpan (InF (App s _ _)) = s

instance HasSpan PosOffset TextSpan where
  getSpan TextSpan{..} = emptyOffsetSpan

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