{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms     #-}

module TextSpan
  ( CharWithPos(..)
  , TextSpan(tsStart, tsEnd, tsText)
  , textSpan, textSpanFrom
  , main
  , parse, termAtPos
  , ExprWithSpan(..), ExprWithSpanF(..), WithSpan(..)
  , pattern GetSpan
  )
where

-- megaparsec
import Text.Megaparsec (Parsec, Stream, VisualStream, TraversableStream, PosState(..))
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as L

-- parser-combinators
import Control.Monad.Combinators.Expr ()
import Control.Applicative hiding (some, many)
import Control.Monad (void, ap)

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Data.Proxy
--import Data.Tuple.Extra
import qualified Data.Char          as C
import qualified Data.Set           as Set
import qualified Data.List.NonEmpty as NE
import Data.Foldable (traverse_)
import Control.Arrow

------------------------------------------------------------

data CharWithPos = CharWithPos
  { cwpPos :: Int
  , cwpChar :: Char
  } deriving (Eq, Show, Ord)

data TextSpan = TextSpan
  { tsStart :: Int -- inclusive from 0
  , tsEnd   :: Int -- exclusive from 0
  , tsText  :: Text
  } deriving (Eq, Show, Ord)

data WithSpan a = WithSpan
  { spanStart :: Int
  , spanEnd   :: Int
  , spanData  :: a
  } deriving (Eq, Show, Functor)

textSpan :: Text -> TextSpan
textSpan = textSpanFrom 0

textSpanFrom :: Int -> Text -> TextSpan
textSpanFrom n t = TextSpan n (T.length t) t

emptyTextSpan :: TextSpan
emptyTextSpan = TextSpan 0 0 T.empty

toSnd :: (a -> b) -> a -> (a,b)
toSnd = ap (,)

instance Stream TextSpan where
  type Token  TextSpan = CharWithPos
  type Tokens TextSpan = TextSpan

  take1_ :: TextSpan -> Maybe (CharWithPos, TextSpan)
  take1_ (TextSpan start end t) = markPos <$> T.uncons t
    where markPos = CharWithPos start *** TextSpan (start+1) end

  takeN_ :: Int -> TextSpan -> Maybe (TextSpan, TextSpan)
  takeN_ n s@(TextSpan start end t)
    | n <= 0    = Just (emptyTextSpan, s)
    | T.null t  = Nothing
    | otherwise = Just (headSpan, restSpan)
        where (head, rest) = T.splitAt n t
              headEnd   = start + T.length head
              headSpan  = TextSpan start headEnd head
              restSpan  = TextSpan headEnd end rest

  -- NOTE: since we fall back to the underlying T.span implementation,
  --   the predicate always receives a dummy position of 0
  takeWhile_ :: (CharWithPos -> Bool) -> TextSpan -> (TextSpan, TextSpan)
  takeWhile_ p (TextSpan a b t) = (TextSpan a fstEnd fst, TextSpan fstEnd b snd)
    where pp :: Char -> Bool
          pp = p . CharWithPos 0 -- TODO (Ben @ 2022/08/16) is it harmful to pass a dummy 0 position here?
          (fst,snd) = T.span pp t
          fstEnd = a + T.length fst

  -- TODO (Ben @ 2022/08/16) this is probably buggy / very inefficient
  -- ideally we would use the underlying T.span function, but that takes a
  -- (Char -> Bool) meaning we would have to feed the predicate a dummy position 
  -- takeWhile_ :: (CharWithPos -> Bool) -> TextSpan -> (TextSpan, TextSpan)
  -- takeWhile_ _ (TextSpan 0 0 _) = (emptyTextSpan, emptyTextSpan)
  -- takeWhile_ p (TextSpan a b t) = finish $ help p a t 
  --   where
  --     help :: (CharWithPos -> Bool) -> Int -> Text -> ([Char], Text)
  --     help p start t =
  --       case T.uncons t of
  --         Nothing -> ([], T.empty)
  --         Just (c, rest)
  --           | p (CharWithPos start c) -> first (c:) $ help p (start+1) rest
  --           | otherwise -> ([], T.empty)
  --     finish :: ([Char], Text) -> (TextSpan, TextSpan)
  --     finish (s,t) = (TextSpan a stEnd st, TextSpan stEnd b t)
  --       where st = T.pack s
  --             stEnd = a + T.length st

  tokensToChunk :: Proxy TextSpan -> [CharWithPos] -> TextSpan
  tokensToChunk Proxy [] = emptyTextSpan
  tokensToChunk Proxy lst@((CharWithPos pos c):cs) = TextSpan pos (length lst) (T.pack $ cwpChar <$> lst)

  chunkToTokens :: Proxy TextSpan -> TextSpan -> [CharWithPos]
  chunkToTokens Proxy (TextSpan a b t) = uncurry CharWithPos <$> zip [a..] (T.unpack t)

  chunkLength :: Proxy TextSpan -> TextSpan -> Int
  chunkLength Proxy (TextSpan a b _) = b - a

------------------------------------------------------------

instance VisualStream TextSpan where
  showTokens p xs = T.unpack . tsText $ ts
    where ts = MP.tokensToChunk (p :: Proxy TextSpan) (NE.toList xs)

------------------------------------------------------------

instance TraversableStream TextSpan where
  reachOffset :: Int -> PosState TextSpan -> (Maybe String, PosState TextSpan)
  reachOffset offset state = (ms, unForget offset pst)
    where (ms, pst) = MP.reachOffset offset (forget state)
          forget :: PosState TextSpan -> PosState Text
          forget ps@PosState{..} = ps{pstateInput = tsText pstateInput}
          unForget :: Int -> PosState Text -> PosState TextSpan
          unForget startPos ps@PosState{..}
            = ps{ pstateInput = TextSpan startPos endPos pstateInput }
            where endPos = startPos + T.length pstateInput

------------------------------------------------------------

type Parser = Parsec Void TextSpan
type ParserWithSpan a = Parser (WithSpan a)

liftCharPredicate :: (Char -> Bool) -> (CharWithPos -> Bool)
liftCharPredicate p = (p . cwpChar)

isSpace = liftCharPredicate C.isSpace
isAlpha = liftCharPredicate C.isAlphaNum
isAlphaNum = liftCharPredicate C.isAlphaNum

space1 :: Parser ()
space1 = void $ MP.takeWhile1P (Just "white space") isSpace
{-# INLINE space1 #-}

------------------------------------------------------------

satisfy :: (Char -> Bool) -> Parser CharWithPos
satisfy p = MP.token test Set.empty
  where test x@(CharWithPos _ t) =
          if p t then Just x
                 else Nothing

string :: Text -> Parser TextSpan
string t = MP.tokens test (textSpan t)
  where test ts1 ts2 = tsText ts1 == tsText ts2

char :: Char -> Parser CharWithPos
char c = satisfy (== c)

-- spaces and newlines
scn :: Parser ()
scn = L.space space1 empty empty

-- spaces only, not newlines
sc :: Parser ()
sc = L.space (void $ MP.some (char ' ' <|> char '\t')) empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc
{-# INLINEABLE lexeme #-}

symbol ::
  Text ->  -- symbol to parse
  Parser TextSpan
symbol = L.lexeme sc . string
{-# INLINEABLE symbol #-}

------------------------------------------------------------

withSpan :: (Text -> a) -> TextSpan -> WithSpan a
withSpan f (TextSpan a b t)
  = WithSpan { spanStart = a, spanEnd = b, spanData = f t }

------------------------------------------------------------

data ExprF a
  = FLeaf Text
  | FNode a a
  deriving (Eq, Show, Functor)

newtype Mu f = InF { outF :: f (Mu f) }

newtype ExprWithSpanF a
  = ExprWithSpanF (WithSpan (ExprF a))
  deriving (Eq, Show, Functor)

type ExprWithSpan = Mu ExprWithSpanF

showSpan :: Int -> Int -> String
showSpan a b = "{" ++ show a ++ "," ++ show b ++ "}"

showOneLayer :: Show a => WithSpan (ExprF a) -> String
showOneLayer (WithSpan a b (FLeaf t)) = T.unpack t ++ showSpan a b
showOneLayer (WithSpan a b (FNode tl tr)) = "(" ++ show tl ++ "," ++ show tr ++ ")" ++ showSpan a b

instance Show (Mu ExprWithSpanF) where
  show (InF (ExprWithSpanF ws)) = showOneLayer ws


exprWithSpan :: Int -> Int -> ExprF (Mu ExprWithSpanF) -> ExprWithSpan
exprWithSpan a b e = InF $ ExprWithSpanF (WithSpan a b e)

getSpan :: ExprWithSpan -> (Int,Int)
getSpan (InF (ExprWithSpanF ws)) = (spanStart ws, spanEnd ws)

------------------------------------------------------------

cons :: (CharWithPos, TextSpan) -> TextSpan
cons (CharWithPos a x, TextSpan b c t) = TextSpan a c (T.cons x t)

name :: Parser TextSpan
name = cons <$> ident
  where ident :: Parser (CharWithPos, TextSpan)
        ident   = (,) <$> MP.satisfy isAlpha <*> MP.takeWhileP Nothing isAlphaNum

pLeaf :: Parser ExprWithSpan
pLeaf = go <$> name
  where go :: TextSpan -> ExprWithSpan
        go (TextSpan a b t) = exprWithSpan a b (FLeaf t)

pNode :: Parser ExprWithSpan
pNode = do
  TextSpan a _ _ <- symbol "("
  left <- pNode <|> pLeaf
  _ <- symbol ","
  right <- pNode <|> pLeaf
  TextSpan _ b _ <- symbol ")"
  return $ exprWithSpan a b (FNode left right)

------------------------------------------------------------

cata :: Functor f => (f b -> b) -> Mu f -> b
cata fn =
  outF                -- peel off one layer of the Expr
  >>> fmap (cata fn)  -- apply `mystery fn` to every subexpr
  >>> fn              -- apply fn to the top-level

ana :: forall b f. Functor f => (b -> f b) -> (b -> Mu f)
ana fn =
  fn                -- wrap b in one more layer
  >>> fmap (ana fn) -- ???
  >>> InF           -- convert the wrapped value to a value of the fix type

anaExpr :: (b -> ExprF b) -> (b -> Mu ExprF)
anaExpr = ana

foo :: Int -> ExprF Int
foo 0 = FLeaf "leaf"
foo n = FNode (n-1) 0

buildTree :: Int -> Mu ExprF
buildTree = ana foo

showExprF :: Show a => ExprF a -> String
showExprF (FLeaf t) = T.unpack t
showExprF (FNode tl tr) = "(" ++ show tl ++ "," ++ show tr ++ ")"

instance Show (Mu ExprF) where
  show (InF e) = showExprF e

------------------------------------------------------------

type Result a = Mu (ResultF a)
data ResultF a b
  = Done a
  | Next b
  deriving (Show, Eq, Functor)

pattern GetSpan x y e = InF (ExprWithSpanF (WithSpan x y e))
pattern GetExpr e <- InF (ExprWithSpanF (WithSpan _ _ e))
pattern GetLeaf t <- InF (ExprWithSpanF (WithSpan _ _ (FLeaf t)))
pattern GetNode l r <- InF (ExprWithSpanF (WithSpan _ _ (FNode l r)))

-- anamorphism
prune :: Int -> ExprWithSpan -> Result ExprWithSpan
prune v = ana go
  where go :: ExprWithSpan -> ResultF ExprWithSpan ExprWithSpan
        go (GetNode l r)
          | la <= v && v < lb = Next l
          | ra <= v && v < rb = Next r
          where GetSpan la lb _ = l
                GetSpan ra rb _ = r
        go expr = Done expr

-- catamorphism
finish :: Result ExprWithSpan -> ExprWithSpan
finish = cata go
  where go :: ResultF ExprWithSpan ExprWithSpan -> ExprWithSpan
        go (Done e) = e
        go (Next e) = e

-- hylomorphism
termAtPos :: Int -> ExprWithSpan -> ExprWithSpan
termAtPos p expr = finish (prune p expr)

------------------------------------------------------------

parseTest p t = MP.parseTest p (textSpan t)

exprName :: ExprWithSpan -> Text
exprName (InF (ExprWithSpanF (WithSpan _ _ (FLeaf t)))) = t
exprName (InF (ExprWithSpanF (WithSpan _ _ (FNode _ _)))) = "node"

testSearch :: Int -> ExprWithSpan -> IO ()
testSearch n expr = do
  let x = [0.. n]
  let y = map (exprName . (`termAtPos` expr)) x
  traverse_ print y

parse :: Text -> Either (MP.ParseErrorBundle TextSpan Void) ExprWithSpan
parse t = do
  let result = MP.runParser pNode "[filename]" (textSpan t)
  result

main :: IO ()
main = do
  let example = "((foo,bar),(baz,(qux,bat))))"
  let n = T.length example
  let result = MP.runParser pNode "" (textSpan example)

  case result of
    Left peb -> do
      putStrLn $ MP.errorBundlePretty peb
    Right expr -> testSearch n expr

  -- print $ buildTree 0
  -- print $ buildTree 1
  -- print $ buildTree 5

  pure ()
