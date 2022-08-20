{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE TupleSections     #-}
module Yagi.Parser where

-- megaparsec
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

-- parser-combinators
import Control.Monad.Combinators.Expr ()

import Control.Applicative hiding (some, many)
import Control.Monad (void, ap, when)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Functor
import Data.Foldable (traverse_)
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))

import Data.Void
import Data.Proxy

import Data.Tuple.Extra

import qualified TextSpan as TS

import Yagi.Lang
import Data.Char
import PrettyPrint

import System.IO


------------------------------------------------------------

type Parser = Parsec Void Text

------------------------------------------------------------

lineComment :: Parser ()
lineComment = L.skipLineComment "--"

-- consume spaces and newlines
scn :: Parser ()
scn = L.space space1 lineComment empty

-- consume spaces only
sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment empty

-- augments the given parser by consuming all trailing whitespace
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- matches the given text and consumes all trailing whitespace
symbol :: Text -> Parser Text
symbol = L.symbol sc

------------------------------------------------------------

keywordFun :: Parser ()
keywordFun = void $ symbol "fun"

symbolColon :: Parser ()
symbolColon = void $ symbol ":"

symbolColonEqual :: Parser ()
symbolColonEqual = void $ symbol ":="

symbolArrow :: Parser ()
symbolArrow = void $ symbol "->"

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

------------------------------------------------------------

pSpine :: Parser Expr
pSpine = foldl1 App <$> some simpleExpr

expr :: Parser Expr
expr = choice
  [ pSpine
  ]

simpleExpr :: Parser Expr
simpleExpr = choice
  [ Var <$> namedSym
  , universe
  , parens expr
  ]

universe :: Parser Expr
universe = Universe <$> p
  where p = symbol "Type" *> lexeme L.decimal

------------------------------------------------------------

reserved :: [Text]
reserved = ["fun", "forall", "Type"]

name :: Parser Text
name = ident >>= check
  where ident   = T.cons <$> satisfy isAlpha <*> takeWhileP Nothing isAlphaNum
        check x = if x `elem` reserved
                  then fail $ "keyword " ++ show x ++ " cannot be used as a variable name"
                  else return x

namedSym :: Parser Symbol
namedSym = NamedSym <$> lexeme name

many1 :: Parser a -> Parser (NonEmpty a)
many1 p = (:|) <$> p <*> many p

------------------------------------------------------------

data BindMany = BindMany
  { bmNames :: [Symbol]
  , bmTpe :: Expr
  } deriving (Eq, Show)

--               names  type
bind1 :: Parser BindMany
bind1 = BindMany <$> vars <*> expr
  where vars = (some namedSym <* symbolColon) :: Parser [Symbol]

--                names  type
binds :: Parser [BindMany]
binds = some (parens bind1)

abstraction :: Parser [BindMany]
abstraction = ((:[]) <$> bind1) <|> binds

------------------------------------------------------------

funBindings :: Parser [BindMany]
funBindings = keywordFun *> abstraction <* symbolArrow

funAbstraction :: Parser Expr
funAbstraction = unrollBinds <$> funBindings <*> expr

unrollBinds :: [BindMany] -> Expr -> Expr
unrollBinds bs expr = foldr unrollBind expr bs

unrollBind :: BindMany -> Expr -> Expr
unrollBind (BindMany xs tpe) = bindMany tpe xs

bindMany :: Expr -> [Symbol] -> Expr -> Expr
bindMany _   []     expr = expr
bindMany tpe (x:xs) expr = Lambda $ Abstraction x tpe (bindMany tpe xs expr)

------------------------------------------------------------

data Directive
  = Assume Symbol Expr  -- add a type assertion to the context
  | Define Symbol Expr  -- add a variable definition to the context
  | Check Expr          -- infer the type of an expression
  | Print Expr          -- pretty print an expression
  | Skip                -- skip line
  | Quit                -- quit interactive mode
  deriving (Show, Eq)

dAssume :: Parser Directive
dAssume = symbol "Assume" *> typeBinding
  where namedSymColon = namedSym <* symbolColon           :: Parser Symbol
        typeBinding   = Assume <$> namedSymColon <*> expr :: Parser Directive

dDefine :: Parser Directive
dDefine = symbol "Define" *> assignment
  where namedSymAssign = namedSym <* symbolColonEqual       :: Parser Symbol
        assignment     = Define <$> namedSymAssign <*> expr :: Parser Directive

dPrint :: Parser Directive
dPrint = symbol "Print" *> (Print <$> expr)

pDirective :: Parser Directive
pDirective = choice
  [ dAssume <* eof
  , dDefine <* eof
  , dPrint  <* eof
  , Skip <$ eof
  , Quit <$ symbol "Quit"
  ]

------------------------------------------------------------

lexNamed = lexeme namedSym
varsThenColon = some lexNamed <* symbolColon

pBind :: Parser BindMany
pBind = BindMany <$> vars <*> expr
  where vars = (some namedSym <* symbolColon) :: Parser [Symbol]

runTest :: Parser Expr -> Text -> IO ()
runTest p t = do
  let result = runParser p "" t
  case result of
    Left peb -> print peb
    Right ex -> T.putStrLn $ prettyPrint ex

processDirective :: Directive -> IO Bool
processDirective (Assume sym expr) = do
  pure True
processDirective (Define sym expr) = do
  pure True
processDirective (Check expr) = do
  pure True
processDirective (Print expr) = do
  T.putStrLn $ prettyPrint expr
  pure True
processDirective Skip = pure True
processDirective Quit = pure False

loopDirective :: IO ()
loopDirective = do
  -- prompt
  putStr "[yagi] " >> hFlush stdout
  -- parse directive
  command <- runParser pDirective "repl" <$> T.getLine
  -- handle directive
  continue <- case command of
    Left peb -> do
      putStrLn $ errorBundlePretty peb
      pure True
    Right d  -> processDirective d
  
  if continue
    then loopDirective
    else pure ()

main :: IO ()
main = do
  parseTest bind1 "x y : t"
  parseTest bind1 "foo bar z : t"
  parseTest namedSym "function"
  parseTest namedSym "Typetyeory"
  parseTest namedSym "fun party"

  runTest funAbstraction "fun (x y z : t) -> x"
  parseTest dAssume "Assume x : Int"

  putStrLn "Please enter a directive!"
  loopDirective

  pure ()