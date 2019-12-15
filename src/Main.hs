module Main where

data Expr = Int Integer
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
  deriving (Eq, Ord, Show, Read)

genExpr :: Expr -> String
genExpr (Int x) = show x
genExpr (Add e1 e2) = parens $ genExpr e1 <> "+" <> genExpr e2
genExpr (Sub e1 e2) = parens $ genExpr e1 <> "-" <> genExpr e2
genExpr (Mul e1 e2) = parens $ genExpr e1 <> "*" <> genExpr e2
genExpr (Div e1 e2) = parens $ genExpr e1 <> "/" <> genExpr e2

parens :: String -> String
parens x = "(" <> x <> ")"

compile :: Expr -> String
compile x = "int main(void) { return " <> genExpr x <> "; }"

main :: IO ()
main = do
  src <- getContents
  let value = read src :: Expr
  putStrLn $ compile value
