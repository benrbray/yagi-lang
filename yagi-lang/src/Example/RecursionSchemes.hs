{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleInstances #-}
module Example.RecursionSchemes where

import Control.Arrow
import Data.Foldable

------------------------------------------------------------

-- Thomson 2014, "Recursion Schemes"
-- https://blog.sumtypeofway.com/posts/recursion-schemes-part-2.html

data ExprF a
  = Literal { intVal :: Int }
  | Ident   { name :: String  }
  | Index   { target :: a, idx :: a }
  | Unary   { op :: String, target :: a }
  | Binary  { lhs :: a, op :: String, rhs :: a }
  | Call    { func :: a, args :: [a] }
  | Paren   { target :: a }
  deriving (Show, Eq, Functor)

------------------------------------------------------------

newtype Mu f = InF { outF :: f (Mu f) }

type Expr = Mu ExprF


-- In :: ExprF Expr -> Expr
-- out :: Expr -> ExprF Expr

-- f-algebra with carrier a
type Algebra f a = f a -> a

x :: Algebra f (Mu f)
x = InF

------------------------------------------------------------

ten :: Expr
ten = InF ( Literal { intVal = 10 } )

add :: Expr
add = InF ( Ident { name = "add" } )

call :: Expr
call = InF ( Call { func = add, args = [ten,ten] } )

--------------------

-- cata = "downwards"
-- suitable for bottom-up recursion with suitably "local" computations
-- "histomorphisms" are like catamorphisms that allow acting upon past history
cata :: Functor f => (f b -> b) -> Mu f -> b
cata fn =
  outF                -- peel off one layer of the Expr
  >>> fmap (cata fn)  -- apply `mystery fn` to every subexpr
  >>> fn              -- apply fn to the top-level

-- Q? when does cata know to stop recursion?
-- A: `cata fn` is the identity over the inhabitants of `f Void`, in this case Literal and Ident

-- anamorphism
ana :: forall b f. Functor f => (b -> f b) -> (b -> Mu f)
ana fn =
  fn                -- wrap b in one more layer
  >>> fmap (ana fn) -- ???
  >>> InF           -- convert the wrapped value to a value of the fix type

cataExpr :: (ExprF b -> b) -> Expr -> b
cataExpr = cata @ExprF

-- ExprF Int -> Int
countNodes :: Algebra ExprF Int
countNodes (Literal _) = 1
countNodes (Ident _) = 1
countNodes (Index target idx) = target + idx + 1
countNodes (Unary _ arg) = 1 + arg
countNodes (Binary left _ right) = left + right + 1
countNodes (Call fn args) = fn + sum args + 1
countNodes (Paren arg) = arg + 1

count :: Expr -> Int
count = cataExpr countNodes

-- laws
-- cata In = id
-- cata (alg >>> fmap func) = (cata alg) >>> func (avoid successive invocations of fmap)
-- cata (f >>> In) >>> cata g = cata (f >>> g)

------------------------------------------------------------

type Bst = Mu BstF
data BstF a
  = BstLeaf
  | BstNode a Int a
  deriving (Eq, Show, Functor)

showBstF :: Show a => BstF a -> String
showBstF BstLeaf = "leaf"
showBstF (BstNode tl v tr) = show v ++ "(" ++ show tl ++ "," ++ show tr ++ ")"

instance Show (Mu BstF) where
  show (InF e) = showBstF e

type Result = Mu ResultF
data ResultF a
  = Done Bool -- success or failure
  | Next a    -- thunk returning the next Result
  deriving (Eq, Show, Functor)

finish :: Result -> Bool
finish = cata go
  where go :: ResultF Bool -> Bool
        go (Done b) = b
        go (Next a) = a

prune :: Int -> Bst -> Result
prune x = ana go
  where go :: Bst -> ResultF Bst
        go (InF BstLeaf) = Done False
        go (InF (BstNode l y r))
          | x < y     = Next l
          | x > y     = Next r
          | otherwise = Done True

-- https://cstheory.stackexchange.com/a/18399/67292
search :: Int -> Bst -> Bool
search x tree = finish (prune x tree)

bstNode :: Bst -> Int -> Bst -> Bst
bstNode x y z = InF $ BstNode x y z

bstLeaf :: Int -> Bst
bstLeaf x = bstNode (InF BstLeaf) x (InF BstLeaf)

bstEmpty = InF BstLeaf

example :: Bst
example = 
  bstNode
    (bstNode
      (bstLeaf 1)
      2
      (bstLeaf 3)
    )
    5
    (bstNode
      (bstLeaf 6)
      10
      (bstLeaf 20)
    )

main :: IO ()
main = do
  let x = [1..20]
  let y = map (`search` example) x
  let z = zip x y

  traverse_ print z
