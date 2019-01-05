import Data.Maybe 

-- some types

-- enum
data FavColor = Blue | Green | Red deriving (Show, Eq)

showColor :: FavColor -> String
showColor c = show c

isColorEq :: FavColor -> FavColor -> Bool
isColorEq c1 c2 = c1 == c2

-- algebraic data types (types multiple value constructors)
type Amount = Int
type Digits = Int

data Billing = CreditCard Digits Digits Digits Digits | Cash Amount

showBillingInfo :: Billing -> String
showBillingInfo binfo = case binfo of
    CreditCard a b c d -> "Credit card ending with digits xxxx xxxx xxxx " ++ show d  
    Cash a -> "Cash $" ++ show a

-- record syntax
data Person = Person { name :: String, age :: Int } deriving (Show, Eq)

displayPerson :: Person -> String
displayPerson p = case p of
    Person name age -> "Hello, my name is " ++ name ++ " and my age is " ++ show age ++ "."

-- parameterized types (generics) + recursive types
data List a = Head a (List a) | Nil deriving (Show)

toHaskellList :: List a -> [a] 
toHaskellList a = case a of
   Nil -> [] 
   Head a b -> a : (toHaskellList b) 

-- with
withdrawAmount :: Double -> Double -> Maybe Double
withdrawAmount balance amt = if amt > balance
                             then Nothing
                             else Just diff
                where diff = balance - amt

main = do
    putStrLn "Types :)"
    putStrLn $ showColor Red
    putStrLn $ show $ isColorEq Red Green
    putStrLn $ show $ isColorEq Red Red
    putStrLn $ showBillingInfo $ CreditCard 111 222 333 444
    putStrLn $ showBillingInfo $ Cash 100
    putStrLn $ displayPerson $ Person "John Doe" 25
    putStrLn $ name $ Person "Jane Doe" 25
    let l1 = Head 10 Nil
    let l2 = Head 10 (Head 20 (Head 30 (Nil))) 
    putStrLn $ show $ toHaskellList l1
    putStrLn $ show $ toHaskellList l2
    putStrLn $ show $ withdrawAmount 100 10
