{-# LANGUAGE OverloadedStrings #-}
module Test where

import qualified TextSpan as TS
import qualified Text.Megaparsec as MP
import Data.Char as C
import Data.Void
import qualified Text.Megaparsec.Error as MP
import Data.Data

import qualified Yagi.ParserNew as P
import Util.PrettyPrint

parseTest p t = do
  let r = MP.runParser p "<file>" (TS.textSpan t)

  case r of
    Left peb -> putStrLn $ MP.errorBundlePretty peb
    Right e -> do
      prettyPrint e

type Parser = MP.Parsec Void TS.TextSpan

p :: Parser TS.TextSpan
p = TS.cons <$> MP.satisfy TS.isAlpha <*> MP.takeWhileP Nothing TS.isAlphaNum

main :: IO ()
main = do
  putStrLn "hello, world!"

  parseTest P.variable "foo bar baz"
  parseTest P.lambdaAbstraction "fun (x y : t) (z : Type0) -> x"

  --parseTest (TS.string "foo\nbar") "foo\nbar baz"
  --parseTest p "foo bar baz"

  -- print $ TS.posFrom (TS.Pos 0 0) ""
  -- print $ TS.posFrom (TS.Pos 0 0) "\n"
  -- print $ TS.posFrom (TS.Pos 3 2) "foo\nbbbb\nbaz\n\nccc"

  pure ()