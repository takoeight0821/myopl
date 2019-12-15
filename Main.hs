module Main where

main :: IO ()
main = do
  src <- getContents
  let value = read src :: Int
  putStrLn $ "int main(void) { return " <> show value <> "; }"
