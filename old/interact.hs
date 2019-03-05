import System.Environment (getArgs)
import Data.Maybe
import Data.Char (toUpper)
import System.IO


data Color = Color { red :: Int, green :: Int, blue :: Int } deriving (Eq, Show, Read)

data RecordFn = RecordFn { name :: String, fn :: Int -> (Color, Int) } 

c1 = Color 255 255 255

r1 = RecordFn { name = "Yohann", fn = \_ -> (c1, 123) }


main = do
    let a = [1,2,3]
    putStrLn (show ((fn r1) 10))
    mapM_ (putStrLn . ("Number " ++) . show) a

