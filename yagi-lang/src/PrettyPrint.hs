{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module PrettyPrint where

import Data.Text (Text)
import qualified Data.Text as T

import YagiLang

showText :: Show a => a -> Text
showText = T.pack . show 

prettySym :: Symbol -> Text
prettySym (NamedSym t) = t
prettySym (SynthSym t i) = T.concat [t, "@", showText i]
prettySym Dummy = "*"

prettyPrint :: Expr -> Text
prettyPrint (Var (NamedSym t))   = t
prettyPrint (Var (SynthSym t i)) = T.concat [t, "@", showText i]
prettyPrint (Var Dummy)          = "*"
prettyPrint (Universe u)         = T.concat ["Type", showText u]
prettyPrint (Pi (Abstraction sym tpe expr))
  = T.concat
      [ "(Π {"  , prettySym sym
      , " : "  , prettyPrint tpe
      , "} ->" , prettyPrint expr, ")"] 
prettyPrint (Lambda (Abstraction sym tpe expr))
  = T.concat
      [ "(λ {"  , prettySym sym
      , " : "  , prettyPrint tpe
      , "}. " , prettyPrint expr, ")"]
prettyPrint (App e1 e2)
  = T.concat
      [ prettyPrint e1, " (", prettyPrint e2, ")"]