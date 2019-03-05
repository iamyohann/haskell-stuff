
clouds :: [Int] -> Int
clouds c = case c of
    (0:1:1:ys) -> 1 + clouds ys
    (0:1:0:ys) -> 1 + clouds (0:ys)
    (0:0:1:ys) -> 1 + clouds (0:1:ys)
    (0:0:0:ys) -> 1 + clouds (0:ys)
    (0:0:_) -> 1
    (1:0:_) -> 1
    _ -> 0
