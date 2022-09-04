module Test where

import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Error as MP
import Data.Char as C
import Data.Void
import Data.Data

import qualified Yagi.Parser as P
import qualified Util.TextSpan as TS
import Util.PrettyPrint

------------------------------------------------------------

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
  parseTest P.lambdaAbstraction "fun (x y : t) (z : Type0) -> forall (q : t), z"
  parseTest P.expr "(fun (x y : t) (z : Type0) -> forall (q : t), z) Type2"

  pure ()