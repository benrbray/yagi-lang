{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text
import Data.Functor.Identity ( Identity(runIdentity) )
import Data.Typeable
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe

-- project imports
import YagiLang

------------------------------------------------------------

main :: IO ()
main = do
  let ctx = Ctx { ctxCounter = 0 }
  (result, ctx) <- runStateT (runExceptT (runLang app)) ctx
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right r  -> print r

printEqualExpr :: TypeCtx -> Expr -> Expr -> Lang ()
printEqualExpr ctx e1 e2 = do
  eq <- equalExpr ctx e1 e2
  liftIO $ print eq

app :: Lang ()
app = do
  let x = NamedSym "x"
  let y = NamedSym "y"
  let z = NamedSym "z"
  let a = NamedSym "a"
  let b = NamedSym "b"
  let c = NamedSym "c"

  let type0 = Universe 0
  let type1 = Universe 0

  let ctx = TypeCtx $ Map.fromList
        [ (x, [SymbolInfo type0 Nothing])
        , (y, [SymbolInfo type0 Nothing])
        ]

  let idX = Lambda (Abstraction x type0 (Var x))
  tpeIdX <- inferType ctx idX

  liftIO $ print idX
  liftIO $ print ""
  liftIO $ print tpeIdX

  printEqualExpr ctx (Var x) (Var y)
  printEqualExpr
    ctx
    (Lambda (Abstraction x type0 (Var x)))
    (Lambda (Abstraction y type0 (Var y)))

  x <- inferType ctx (App idX (Var x))
  liftIO $ print x

  pure ()

data TypeError
  = ErrorUnknownIdentifier Symbol
  | ErrorExpectedUniverse Expr
  | ErrorExpectedFunctionType Expr
  | ErrorExpectedEqualTypes Expr Expr
  deriving Show

newtype Ctx = Ctx {
  ctxCounter :: Int -- used to generate fresh symbols
}

type MonadTypeError = MonadError TypeError
type MonadSymGen = MonadState Ctx

newtype Lang a = Lang {
  runLang :: ExceptT TypeError (StateT Ctx IO) a
} deriving (Functor, Applicative, Monad, MonadState Ctx, MonadIO, MonadError TypeError)

------------------------------------------------------------

class SymbolGenerator m where
  freshSymbol :: Symbol -> m Symbol

instance MonadState Ctx m => SymbolGenerator m where
  freshSymbol sym = do
    k <- gets ctxCounter
    modify (\x -> x { ctxCounter = k + 1 })
    return $ case sym of
      NamedSym name   -> SynthSym name k
      SynthSym name _ -> SynthSym name k
      Dummy           -> SynthSym "_" k

---- substitution ------------------------------------------

type Subst = [(Symbol, Expr)]

subst :: MonadState Ctx m => Subst -> Expr -> m Expr
subst s (Var x) = pure $ case lookup x s of
  Nothing -> Var x
  Just r -> r
subst s (Universe u) = pure $ Universe u
subst s (Pi a) = Pi <$> substAbstraction s a
subst s (Lambda a) = Lambda <$> substAbstraction s a
subst s (App e1 e2) = do
  e1 <- subst s e1
  e2 <- subst s e2
  return $ App e1 e2

substAbstraction :: MonadState Ctx m => Subst -> Abstraction -> m Abstraction
substAbstraction s Abstraction{..} = do
  r    <- freshSymbol absSym
  tpe  <- subst s absType
  expr <- subst ((absSym, Var r) : s) absExpr
  return $ Abstraction r tpe expr

---- type inference ----------------------------------------

data SymbolInfo = SymbolInfo {
  symType :: Expr,      -- symbol has a type
  symVal :: Maybe Expr  -- symbol may or may not have an assigned definition
}

newtype TypeCtx = TypeCtx { types :: Map Symbol [SymbolInfo] }

lookupSymbol :: Symbol -> TypeCtx -> Maybe SymbolInfo
lookupSymbol c ctx = listToMaybe (Map.findWithDefault [] c (types ctx))

-- | returns the type of symbol `x` in context `ctx`
lookupTpe :: MonadTypeError m => Symbol -> TypeCtx -> m Expr
lookupTpe s ctx =
  case lookupSymbol s ctx of
    Nothing -> throwError (ErrorUnknownIdentifier s)
    Just si -> pure $ symType si


-- | returns the value of symbol `x` in context `ctx`
lookupVal :: MonadTypeError m => Symbol -> TypeCtx -> m (Maybe Expr)
lookupVal s ctx =
  case lookupSymbol s ctx of
    Nothing -> throwError (ErrorUnknownIdentifier s)
    Just si -> pure $ symVal si

-- | prepends value to the corresponding entry if it exists, otherwise creates the entry
upsert :: forall k v. Ord k => k -> v -> Map k [v] -> Map k [v]
upsert key val = Map.alter (Just . go) key
  where go :: Maybe [v] -> [v]
        go Nothing = [val]
        go (Just x) = val:x

-- | extends the context with variable `x` of type `t`
extendCtx :: Symbol -> Expr -> Maybe Expr -> TypeCtx -> TypeCtx
extendCtx x t e TypeCtx{..} = TypeCtx $ upsert x (SymbolInfo t e) types

-- | infers the type of expression `e` in context `ctx`
inferType :: (MonadSymGen m, MonadTypeError m) => TypeCtx -> Expr -> m Expr
inferType ctx (Var x) = lookupTpe x ctx
inferType ctx (Universe u) =
  pure $ Universe (u+1)
inferType ctx (Pi (Abstraction x t1 t2)) = do
  k1 <- inferUniverse ctx t1
  k2 <- inferUniverse (extendCtx x t1 Nothing ctx) t2
  return $ Universe (max k1 k2)
inferType ctx (Lambda (Abstraction x t e)) = do
  te <- inferType (extendCtx x t Nothing ctx) e
  return $ Pi $ Abstraction x t te
inferType ctx (App e1 e2) = do
  Abstraction x tx ex <- checkPi ctx e1
  t2 <- inferType ctx e2
  checkEqual ctx t2 tx
  subst [(x,e2)] tx

-- infers the current universe level of type [t] in context [ctx]
inferUniverse :: (MonadSymGen m, MonadTypeError m) => TypeCtx -> Expr -> m Int
inferUniverse ctx t = do
  tt <- inferType ctx t
  case tt of
    Universe k -> pure k
    _ -> throwError (ErrorExpectedUniverse tt)

-- checks the type of [e] in context [ctx] has the form [Pi (Abstraction x t1 t2)]
checkPi :: (MonadSymGen m, MonadTypeError m) => TypeCtx -> Expr -> m Abstraction
checkPi ctx e = do
  t <- inferType ctx e
  n <- normalize ctx t
  case n of
    Pi a -> pure a
    _ -> throwError (ErrorExpectedFunctionType n)

---- normalization -----------------------------------------

-- normalizes expression `e` in context `ctx
-- removes all redexes and unfolding all definitions
-- performs normalization under binders
normalize :: (MonadSymGen m, MonadTypeError m) => TypeCtx -> Expr -> m Expr
normalize ctx (Var x) =
  lookupVal x ctx >>= \case
    Nothing -> pure $ Var x
    Just e -> normalize ctx e
normalize ctx (App e1 e2) = do
  n2 <- normalize ctx e2
  n1 <- normalize ctx e1
  case n1 of
    Lambda (Abstraction s _ e) -> do
      se <- subst [(s,e2)] e
      normalize ctx se
    e1 -> pure $ App e1 e2
normalize ctx (Universe u) =
  pure $ Universe u
normalize ctx (Pi a) =
  Pi <$> normalizeAbstraction ctx a
normalize ctx (Lambda a) =
  Lambda <$> normalizeAbstraction ctx a

normalizeAbstraction :: (MonadSymGen m, MonadTypeError m) => TypeCtx -> Abstraction -> m Abstraction
normalizeAbstraction ctx Abstraction{..} = do
  nt <- normalize ctx absType
  let extCtx = extendCtx absSym nt Nothing ctx
  ne <- normalize extCtx absExpr
  return $ Abstraction {
    absSym = absSym,
    absType = nt,
    absExpr = ne
  }

---- equality ----------------------------------------------

-- determines whether normalized [e1] and [e2] are
-- judgementally equal up to renaming of bound variables
equalExpr :: (MonadSymGen m, MonadTypeError m) => TypeCtx -> Expr -> Expr -> m Bool
equalExpr ctx e1 e2 = do
  ne1 <- normalize ctx e1
  ne2 <- normalize ctx e2
  equal ne1 ne2

  where equal :: (MonadSymGen m) => Expr -> Expr -> m Bool
        equal e1 e2 =
          case (e1, e2) of
            (Var x1, Var x2)           -> pure $ x1 == x2
            (Universe u1, Universe u2) -> pure $ u1 == u2
            (Lambda a1, Lambda a2)     -> equalAbstraction a1 a2
            (Pi a1, Pi a2)             -> equalAbstraction a1 a2
            (App e11 e12, App e21 e22) -> do { a <- equal e11 e21
                                             ; b <- equal e12 e22
                                             ; return $ a && b    }
            (e1, e2)                   -> pure False

        equalAbstraction :: (MonadSymGen m) => Abstraction -> Abstraction -> m Bool
        equalAbstraction (Abstraction x t1 e1) (Abstraction y t2 e2) = do
          equalTypes <- equal t1 t2
          s2 <- subst [(y, Var x)] e2
          equalExprs <- equal e1 s2
          return $ equalTypes && equalExprs

checkEqual :: (MonadSymGen m, MonadTypeError m) => TypeCtx -> Expr -> Expr -> m ()
checkEqual ctx e1 e2 = do
  eq <- equalExpr ctx e1 e2
  unless eq $ throwError (ErrorExpectedEqualTypes e1 e2)

