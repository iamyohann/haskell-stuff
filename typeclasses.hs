-- typeclasses.hs

data Person = Person { name :: String, age :: Int }

class BasicEq a where
  isEqual :: a -> a -> Bool

instance BasicEq Bool where
  isEqual True True = True
  isEqual False False = True
  isEqual _ _ = False

instance BasicEq Person where
  isEqual a b = (name a) == (name b)

class CustomShow a where
  myShow :: a -> String

instance CustomShow Person where
  myShow a = "Person [ name: " ++ (name a) ++ " age: " ++ show (age a) ++ "]"

main = do
  let a = Person { name = "Foo", age = 100 }
  let b = Person { name = "Foo", age = 10 }
  let c = Person { name = "Bar", age = 100 }
  putStrLn $ show $ isEqual a b
  putStrLn $ show $ myShow a
  putStrLn $ show $ isEqual a c
  putStrLn $ show $ isEqual True True
  putStrLn $ show $ isEqual True False
  putStrLn $ show $ isEqual False True
  putStrLn $ show $ isEqual False False
  putStrLn "typeclasses"
