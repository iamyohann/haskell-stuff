import Data.Maybe
import Data.List
import System.Environment (getArgs)

prettyArg :: (Integer, String) -> String
prettyArg (n, arg) = "Arg [" ++ show n ++ "] => " ++ arg

prettyArgs :: [String] -> String
prettyArgs args = case zip [1..] args of
  [] -> "No args passed\n"
  (x:xs) -> intercalate "\n" $ map prettyArg (x:xs)

main = do
  args <- getArgs
  let out = prettyArgs args
  putStrLn "Specify an output file name:"
  fname <- getLine
  writeFile fname out
  print args 
