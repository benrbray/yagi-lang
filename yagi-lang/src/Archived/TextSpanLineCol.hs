{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms   #-}

module TextSpanLineCol
  ( Pos(..), CharWithPos(..), TextSpan(..)
  , posFrom
  , textSpan, textSpanFrom
  , parse, termAtPos
  , ExprWithSpan(..), ExprWithSpanF(..), WithSpan(..)
  , pattern GetSpan
  , cons
  , lexeme, symbol, satisfy, name, string
  , decimal
  , isAlpha, isAlphaNum
  )
where

-- megaparsec
import Text.Megaparsec (
    Parsec, Stream, VisualStream, TraversableStream,
    PosState(..),
    (<?>)
  )
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as L

-- parser-combinators
import Control.Monad.Combinators.Expr ()
import Control.Applicative hiding (some, many)
import Control.Monad (void, ap)

-- text
import Data.Text (Text)
import qualified Data.Text as T
import Data.String (IsString(..))
import qualified Data.Char as C

-- misc
import Data.Void
import Data.Proxy
--import Data.Tuple.Extra
import Control.Arrow
import Data.Foldable ( Foldable(foldl'), traverse_ )
import qualified Data.Set           as Set
import qualified Data.List.NonEmpty as NE

-- yagi-lang
import Yagi.RecursionSchemes

---- helpers (move these?) ---------------------------------

toSnd :: (a -> b) -> a -> (a,b)
toSnd = ap (,)

------------------------------------------------------------

data Pos = Pos
  { posLine :: !Int
  , posCol  :: !Int
  } deriving (Eq, Ord)

instance Show Pos where
  show :: Pos -> String
  show Pos{..} = "l" ++ show posLine ++ "c" ++ show posCol

posEmpty :: Pos
posEmpty = Pos 0 0

posSingle :: Pos
posSingle = Pos 0 1

------------------------------------------------------------

data CharWithPos = CharWithPos
  { cwpPos :: Pos
  , cwpChar :: Char
  } deriving (Eq, Ord)

instance Show CharWithPos where
  show :: CharWithPos -> String
  show CharWithPos{..} = show cwpChar ++ "@" ++ show cwpPos 

{- `Text` annotated with `start` and `end` positions, in terms of lines and
   columns.  All operations on a `TextSpan` must ensure that the positions
   are kept in sync with the value of the text itself.

   TODO (Ben @ 2022/08/22) This is somewhat expensive to manage, since we cannot
   rely on `T.length`, but instead must search the string for newlines.  I am
   interested in comparing this approach with a few others:

    * implementation with `Data.Text.Lines` from `text-rope
    * call `getSourcePos` on a regular Text Stream
    * use a single `Int` position in `TextSpan`, and convert to line/col
        in a post-processing step after the initial parsing
-}
data TextSpan = TextSpan
  { tsStart :: Pos  -- inclusive from 0
  , tsEnd   :: Pos  -- exclusive from 0
  , tsText  :: Text -- INVARIANT:  
  } deriving (Eq, Show, Ord)

instance IsString TextSpan where
  fromString :: String -> TextSpan
  fromString s = textSpan $ T.pack s

-- | create a `TextSpan` starting at the given position
textSpanFrom :: Pos -> Text -> TextSpan
textSpanFrom start t = TextSpan start (posFrom start t) t

-- | O(2n) computes the ending position of `t`, assuming `t` begins at `start`
-- FIXME (Ben @ 2022/08/23) this is buggy for at least one reason (maybe more!)
--       `T.lines "\n" = [""]`, instead of the expected `["",""]` 
posFrom :: Pos -> Text -> Pos
posFrom pos "" = pos
posFrom (Pos l c) t = Pos {..}
  where lines = T.lines t
        posLine = l + length lines - 1
        posCol  = T.length $ last lines

-- | create a `TextSpan` starting from (line 0, column 0)
textSpan :: Text -> TextSpan
textSpan = textSpanFrom (Pos 0 0 )

emptyTextSpan :: TextSpan
emptyTextSpan = textSpan T.empty

cons :: CharWithPos -> TextSpan -> TextSpan
cons (CharWithPos a x) (TextSpan b c t) = TextSpan a c (T.cons x t)

------------------------------------------------------------

incrCol :: Pos -> Pos
incrCol Pos{..} = Pos posLine (posCol + 1)

incrLine :: Pos -> Pos
incrLine Pos{..} = Pos (posLine + 1) 0

posAfter :: CharWithPos -> Pos
posAfter (CharWithPos p '\n') = incrLine p
posAfter (CharWithPos p _)    = incrCol p

------------------------------------------------------------

instance Stream TextSpan where
  type Token  TextSpan = CharWithPos
  type Tokens TextSpan = TextSpan

  take1_ :: TextSpan -> Maybe (CharWithPos, TextSpan)
  take1_ (TextSpan start end t) = markPos <$> T.uncons t
    where 
      markPos :: (Char, Text) -> (CharWithPos, TextSpan)
      markPos (c,t) = (CharWithPos start c, TextSpan next end t)
        where next = case c of
                      '\n' -> incrLine start
                      _    -> incrCol  start

  takeN_ :: Int -> TextSpan -> Maybe (TextSpan, TextSpan)
  takeN_ n s@(TextSpan start end t)
    | n <= 0    = Just (emptyTextSpan, s)
    | T.null t  = Nothing
    | otherwise = Just (headSpan, restSpan)
        where (head, rest) = T.splitAt n t
              headEnd   = posFrom start head
              headSpan  = TextSpan start headEnd head
              restSpan  = TextSpan headEnd end rest

  -- TODO (Ben @ 2022/08/16) is this implementation safe? since we fall back
  -- to the underlying `T.span` implementation, the predicate always receives
  -- a dummy position of 0 
  takeWhile_ :: (CharWithPos -> Bool) -> TextSpan -> (TextSpan, TextSpan)
  takeWhile_ p (TextSpan start end t) = (TextSpan start fstEnd fst, TextSpan fstEnd end snd)
    where pp :: Char -> Bool
          pp = p . CharWithPos posSingle
          (fst,snd) = T.span pp t
          fstEnd = posFrom start fst

  tokensToChunk :: Proxy TextSpan -> [CharWithPos] -> TextSpan
  tokensToChunk Proxy [] = emptyTextSpan
  tokensToChunk Proxy lst@((CharWithPos start _):_) = TextSpan start end (T.pack $ cwpChar <$> lst)
    where go :: CharWithPos -> CharWithPos -> CharWithPos
          go cwp1 (CharWithPos _ c) = CharWithPos (posAfter cwp1) c
          CharWithPos end _ = foldr1 go lst 

  chunkToTokens :: Proxy TextSpan -> TextSpan -> [CharWithPos]
  chunkToTokens Proxy (TextSpan start _ t) =
    case T.uncons t of
      Nothing -> []
      Just (c,t) -> step (CharWithPos start c) t
    where
      step :: CharWithPos -> Text -> [CharWithPos]
      step cwp t =
        case T.uncons t of
          Nothing    -> [cwp]
          Just (c,t) -> cwp : step (CharWithPos (posAfter cwp) c) t

  -- NOTE: relies on the invariant that `TextSpan` positions match actual text
  chunkLength :: Proxy TextSpan -> TextSpan -> Int
  chunkLength Proxy (TextSpan _ _ t) = T.length t

------------------------------------------------------------

instance VisualStream TextSpan where
  showTokens p xs = T.unpack . tsText $ ts
    where ts = MP.tokensToChunk (p :: Proxy TextSpan) (NE.toList xs)

-- ------------------------------------------------------------

instance TraversableStream TextSpan where
  reachOffset :: Int -> PosState TextSpan -> (Maybe String, PosState TextSpan)
  reachOffset offset state@(PosState (TextSpan _ end _) _ _ _ _) = (ms, unForget offset pst)
    where (ms, pst) = MP.reachOffset offset (forget state)
          forget :: PosState TextSpan -> PosState Text
          forget ps@PosState{..} = ps{pstateInput = tsText pstateInput}
          unForget :: Int -> PosState Text -> PosState TextSpan
          unForget offset ps@(PosState t _ (MP.SourcePos _ l c) _ _)
            = ps{ pstateInput = TextSpan start end t }
            where start = Pos (MP.unPos l) (MP.unPos c)

-- ------------------------------------------------------------

type Parser = Parsec Void TextSpan

liftCharPredicate :: (Char -> Bool) -> (CharWithPos -> Bool)
liftCharPredicate p = p . cwpChar

isSpace = liftCharPredicate C.isSpace
isAlpha = liftCharPredicate C.isAlphaNum
isAlphaNum = liftCharPredicate C.isAlphaNum

space1 :: Parser ()
space1 = void $ MP.takeWhile1P (Just "white space") isSpace
{-# INLINE space1 #-}

-- ------------------------------------------------------------

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

decimal :: (Num a) => Parser (TextSpan, a)
decimal = decimal_ <?> "integer"
{-# INLINEABLE decimal #-}

-- | A non-public helper to parse decimal integers.
--   (adapted from `Text.Megaparsec.Char.Lexer`)
decimal_ :: (Num a) => Parser (TextSpan, a)
decimal_ = toSnd mkNum <$> digits
  where
    digits :: Parser TextSpan
    digits = MP.takeWhile1P (Just "digit") (liftCharPredicate C.isDigit)
    mkNum = foldl' step 0 . MP.chunkToTokens (Proxy :: Proxy TextSpan)
    step a c = a * 10 + fromIntegral ((C.digitToInt . cwpChar) c)

------------------------------------------------------------

data WithSpan a = WithSpan
  { spanStart :: Pos
  , spanEnd   :: Pos
  , spanData  :: a
  } deriving (Eq, Show, Functor)

withSpan :: (Text -> a) -> TextSpan -> WithSpan a
withSpan f (TextSpan a b t)
  = WithSpan { spanStart = a, spanEnd = b, spanData = f t }

type ParserWithSpan a = Parser (WithSpan a)

------------------------------------------------------------

data ExprF a
  = FLeaf Text
  | FNode a a
  deriving (Eq, Show, Functor)

newtype ExprWithSpanF a
  = ExprWithSpanF (WithSpan (ExprF a))
  deriving (Eq, Show, Functor)

type ExprWithSpan = Mu ExprWithSpanF

showSpan :: Pos -> Pos -> String
showSpan a b = "{" ++ show a ++ "," ++ show b ++ "}"

showOneLayer :: Show a => WithSpan (ExprF a) -> String
showOneLayer (WithSpan a b (FLeaf t)) = T.unpack t ++ showSpan a b
showOneLayer (WithSpan a b (FNode tl tr)) = "(" ++ show tl ++ "," ++ show tr ++ ")" ++ showSpan a b

instance Show (Mu ExprWithSpanF) where
  show (InF (ExprWithSpanF ws)) = showOneLayer ws


exprWithSpan :: Pos -> Pos -> ExprF (Mu ExprWithSpanF) -> ExprWithSpan
exprWithSpan a b e = InF $ ExprWithSpanF (WithSpan a b e)

getSpan :: ExprWithSpan -> (Pos,Pos)
getSpan (InF (ExprWithSpanF ws)) = (spanStart ws, spanEnd ws)

-- ------------------------------------------------------------

name :: Parser TextSpan
name = cons <$> MP.satisfy isAlpha <*> MP.takeWhileP Nothing isAlphaNum

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

-- ------------------------------------------------------------

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

-- ------------------------------------------------------------

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
prune :: Pos -> ExprWithSpan -> Result ExprWithSpan
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
termAtPos :: Pos -> ExprWithSpan -> ExprWithSpan
termAtPos p expr = finish (prune p expr)

-- ------------------------------------------------------------

-- parseTest p t = MP.parseTest p (textSpan t)

-- exprName :: ExprWithSpan -> Text
-- exprName (InF (ExprWithSpanF (WithSpan _ _ (FLeaf t)))) = t
-- exprName (InF (ExprWithSpanF (WithSpan _ _ (FNode _ _)))) = "node"

-- testSearch :: Int -> ExprWithSpan -> IO ()
-- testSearch n expr = do
--   let x = [0.. n]
--   let y = map (exprName . (`termAtPos` expr)) x
--   traverse_ print y

parse :: Text -> Either (MP.ParseErrorBundle TextSpan Void) ExprWithSpan
parse t = do
  let result = MP.runParser pNode "[filename]" (textSpan t)
  result

-- main :: IO ()
-- main = do
--   let example = "((foo,bar),(baz,(qux,bat))))"
--   let n = T.length example
--   let result = MP.runParser pNode "" (textSpan example)

--   case result of
--     Left peb -> do
--       putStrLn $ MP.errorBundlePretty peb
--     Right expr -> testSearch n expr

--   -- print $ buildTree 0
--   -- print $ buildTree 1
--   -- print $ buildTree 5

--   pure ()
