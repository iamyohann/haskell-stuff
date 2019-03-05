import qualified Data.List as LL

filterA :: String -> String
filterA = LL.filter (=='a')

rp :: String -> Integer -> Integer
rp s n = (quot n (fromIntegral $ length s) * (fromIntegral $ length $ filterA s)) + (fromIntegral $ length $ filterA $ take (fromIntegral $ rem n (fromIntegral $ length s)) s)


-- test cases
-- "aba" 1000000
-- "aba" 1
-- "aba" 1000
-- 999 * 1 + 