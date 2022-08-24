{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Yagi.Syntax where


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

---- simple constructors -----------------------------------

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

---- pretty print ------------------------------------------

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


---- position annotations ----------------------------------

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