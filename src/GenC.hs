{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
module GenC where

import           Control.Monad.State

newtype GenState =
  GenState { stmts :: [String]
           } deriving (Show)

type GenCT m a = StateT GenState m a

genC :: Monad m => GenCT m () -> m String
genC m = do
  GenState { stmts } <- execStateT m (GenState [])
  pure $ "int main(void) {" <> mconcat stmts <> "}"

assign :: MonadState GenState m => String -> String -> m String
assign name value = do
  modify (\s -> s { stmts = ("int " <> name <> " = " <> value <> ";") : stmts s })
  pure name

ret :: MonadState GenState m => String -> m ()
ret value =
  modify (\s -> s { stmts = ("return " <> value <> ";") : stmts s })
