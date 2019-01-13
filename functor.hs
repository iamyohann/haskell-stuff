
data Pair a = Pair a a deriving (Show)

p = Pair 10 10

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

main = do
  putStrLn $ show p
  putStrLn $ show $ fmap (+2) p
  putStrLn $ show $ fmap (**2) p
  putStrLn $ show $ fmap ( (++) "flipped num = " . reverse . show) p
  
  putStrLn "Functor"
