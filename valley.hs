
calcHeight :: Char -> Integer
calcHeight 'D' = -1
calcHeight 'U' = 1
calcHeight _ = 0

s = "UDDDUDUU"

heights = map calcHeight
path = scanl (+) 0

pathHeights = path . heights

valleys :: [Integer] -> Integer
valleys p = case p of
    (-1:0:xs) -> 1 + (valleys xs)
    (_:xs) -> 0 + (valleys xs)
    _ -> 0