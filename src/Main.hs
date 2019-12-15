{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.Functor.Identity
import Control.Monad.State

data Expr = Int Integer
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Var String
          | Let String Expr Expr
  deriving (Eq, Ord, Show, Read)

type GenState = [String]

genExpr :: MonadState [String] m => Expr -> m String
genExpr (Int x) = pure $ show x
genExpr (Add e1 e2) = parens $ do
  e1' <- genExpr e1
  e2' <- genExpr e2
  pure $ e1' <> "+" <> e2'
genExpr (Sub e1 e2) = parens $ do
  e1' <- genExpr e1
  e2' <- genExpr e2
  pure $ e1' <> "-" <> e2'
genExpr (Mul e1 e2) = parens $ do
  e1' <- genExpr e1
  e2' <- genExpr e2
  pure $ e1' <> "*" <> e2'
genExpr (Div e1 e2) = parens $ do
  e1' <- genExpr e1
  e2' <- genExpr e2
  pure $ e1' <> "/" <> e2'
genExpr (Var x) = pure x
genExpr (Let x v e) = do
  v' <- genExpr v
  modify (<> ["int " <> x <> " = " <> v' <> ";"])
  genExpr e

parens :: Monad m => m String -> m String
parens x = x >>= \x' -> pure $ "(" <> x' <> ")"

compile :: Expr -> String
compile x =
  let (ret, lets) = runState (genExpr x) []
  in "int main (void) { " <> mconcat lets <> "return " <> ret <> "; }"

main :: IO ()
main = do
  src <- getContents
  let value = read src :: Expr
  putStrLn $ compile value
