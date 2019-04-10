import qualified Data.List as LL

frequency :: (Eq a) => a -> [a] -> Int
frequency x xs = length $ filter (==x) xs

socks xs = sum $ map ((flip quot) 2) $ [frequency sock xs | sock <- (LL.nub xs)]



sockInput = [10, 20, 20, 10, 10, 30, 50, 10, 20]
