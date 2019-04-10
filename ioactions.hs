import Control.Monad

-- main = do
--     colors <- forM [1..5] (\n -> do
--         putStrLn $ "What is your favourite color #" ++ (show n) ++ "?"
--         color <- getLine
--         return color)
    
--     putStrLn "Thanks"
--     putStrLn $ "Your colors are: " ++ (show colors)


main = do
    putStrLn $ "Say something..."
    str <- fmap reverse getLine
    putStrLn $ "The reverse is: "
    putStrLn $ str
    
