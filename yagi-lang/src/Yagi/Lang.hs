module Yagi.Lang where

import Data.Text (Text)

------------------------------------------------------------

data Symbol
  = NamedSym Text      -- variables named in the source code
  | SynthSym Text Int  -- for variable substitution
  | Dummy              -- used only for pretty printing
  deriving (Show, Eq, Ord)

data Abstraction
  = Abstraction {
      absSym  :: Symbol, -- symbol sym
      absType :: Expr,   -- of type tpe
      absExpr :: Expr    -- is bound in expression ctx
    }
  deriving (Show, Eq)

data Expr
  = Var Symbol
  | Universe Int
  | Pi Abstraction
  | Lambda Abstraction
  | App Expr Expr
  deriving (Show, Eq)