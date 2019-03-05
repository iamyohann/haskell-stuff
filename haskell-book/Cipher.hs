module Cipher where

import qualified Data.Char as Char

shiftAlpha :: Char -> Int -> Char
shiftAlpha c k = Char.chr $ (Char.ord c) + (mod k 26)

caesar :: Int -> String -> String
caesar k = map ((flip shiftAlpha) k)