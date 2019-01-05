

drop' n xs = if n <= 0 || null xs
             then xs
             else drop' (n - 1) (tail xs)

main = do
    let a = [1..100]
    putStrLn $ show $ drop' 10 a
