module Example.ParseUrls where

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

import Data.Text (Text)
import Data.Text as T
import Data.Foldable (traverse_)
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Void

------------------------------------------------------------

examples =
  [ "www.foo.com"
  , "https://"
  , "http//"
  , "https://news.ycombinator.com"
  , "https://mark:@example.com@123"
  ]

main :: IO ()
main = do
  putStrLn "hello, world!"
  traverse_ (parseTest pUri) examples
  print $ runParser pUri "filename.txt" "https://news.ycombinator.com"

------------------------------------------------------------

type Parser = Parsec Void Text

data Scheme
  = SchemeData | SchemeFile | SchemeFtp | SchemeHttp
  | SchemeHttps | SchemeIrc | SchemeMailto
  deriving (Eq, Show)

data Authority = Authority
  { authUser :: Maybe (Text, Text) -- (user, password)
  , authHost :: Text
  , authPort :: Maybe Int
  } deriving (Eq, Show)

data Uri = Uri
  { uriScheme :: Scheme
  , uriAuthority :: Maybe Authority
  } deriving (Eq, Show)

------------------------------------------------------------


x :: Parser Char
x = satisfy (== 'a')

y :: Parser (Char, Char, Char)
y = do
  a <- char 'a'
  b <- char 'b'
  c <- char 'c'
  return (a,b,c)

alphaNumStr :: Parser Text
alphaNumStr = T.pack <$> some alphaNumChar

optionalTry :: MonadParsec e s m => m a -> m (Maybe a)
optionalTry = optional . try

------------------------------------------------------------

pScheme :: Parser Scheme
pScheme = choice
  [ SchemeData   <$ string "data"
  , SchemeFile   <$ string "file"
  , SchemeFtp    <$ string "ftp"
  , SchemeHttps  <$ string "https"
  , SchemeHttp   <$ string "http"
  , SchemeIrc    <$ string "irc"
  , SchemeMailto <$ string "mailto" ]

pAuthority :: Parser Authority
pAuthority = do
  _ <- string "//"
  authUser <- optionalTry $ do
    user <- alphaNumStr <?> "username"
    _ <- char ':'
    password <- alphaNumStr <?> "password"
    _ <- char '@'
    return (user,password)
  authHost <- T.pack <$> some (alphaNumChar <|> char '.') <?> "hostname"
  authPort <- optional (char ':' *> label "port number" L.decimal)
  return $ Authority {..}

pUri :: Parser Uri
pUri = do
  r <- pScheme <?> "valid scheme"
  _ <- char ':'
  a <- optional pAuthority
  return (Uri r a)

------------------------------------------------------------

sc :: Parser ()
sc = L.space
  space1                         -- (2)
  (L.skipLineComment "//")       -- (3)
  (L.skipBlockComment "/*" "*/") -- (4)