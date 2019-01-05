-- basics of haskell

drop' n xs = if n <= 0 || null xs
             then xs
             else drop' (n - 1) (tail xs)

dropcase n xs = case (n <= 0 || null xs) of
    True -> xs
    _ -> drop' (n - 1) (tail xs)


take' n xs = if n <= 0 || null xs
    then [] 
    else [head xs] ++ (take' (n - 1) (tail xs))

last' :: [x] -> Maybe x
last' x = case x of
    [] -> Nothing 
    [x] -> Just x
    (x:xs) -> last' xs

main = do
    let a = [1..100]
    putStrLn $ show $ last' a
    putStrLn $ show $ take' 10 a
    putStrLn $ show $ drop' 10 a
    putStrLn $ show $ dropcase 10 a
