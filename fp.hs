
main = do
  putStrLn $ show $ takeWhile odd [1,3,5,10,11,12]
  putStrLn $ show $ dropWhile odd [1,3,5,10,11,12]
  putStrLn $ show $ all odd [1..10]
  putStrLn $ show $ any odd [1..10] 
