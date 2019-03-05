
module Foo (
) where

    import System.Environment (lookupEnv)
    import Data.Maybe
    import Data.List
    import Control.Exception
    
    data Person = Person { name :: String, age :: Int }

    instance Eq Person where
        p1 == p2 = age p1 == age p2

    
    class YesNo a where
        yesno :: a -> String

    instance YesNo Int where
        yesno 0 = "Nah"
        yesno _ = "Yep"

    -- let f = Foo Bar
    main = do
        5 + 1