module Lib
   ( sayHello, multi, waxOn, 
     triple, topLevelFunction, 
     area, numInString, 
     numInString2, changeMood, isPalindrome,
     myAbs, fn, f1
    ) where

type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

data Rocks = Rocks String deriving (Eq, Show) 
data Yeah = Yeah Bool  deriving (Eq, Show)
data Papu = Papu Rocks Yeah  deriving (Eq, Show)

test :: Integer
test = 10

g :: (a ->b) -> (a, c) ->(b, c)
g f (x, y) = (f x, y)

cattyConny :: String -> String -> String
flippy :: String -> String -> String
flippy = flip cattyConny
appedCatty ::String -> String
appedCatty = cattyConny "woops"
cattyConny x y = x ++ " mrow " ++ y

summer :: (Eq a, Num a) => a->a
summer 0 = 0
summer n = n + summer (n - 1)

-- 5 * 4 == 5 + 5 + 5 + 5; add x to x, (y - 1 times)
mult' x 0 = 0
mult' x y = x + mult' x (y - 1)

foldBool :: a -> a-> Bool ->a
foldBool x y z = 
   case z of
     True -> x
     _    -> y

foldBool3 :: a -> a ->Bool -> a
foldBool3 x y pred
          | pred == True = x
          | otherwise = y
  

pal xs 
    | xs == reverse xs  = True
    | otherwise         = False

mm :: (a -> c) -> a -> a
mm = undefined

f::(a,b,c) -> (d,e,f) ->((a,d), (c,f))
f (a,b,c) (d,e,f) = ((a,d), (c,f))

function' x y = if (x>y) then x else y
function'' x y = 
   case (x > y) of
     True -> x
     _    -> y

ifEvenAdd2 n = if even n then (n + 2) else n
ifEvenAdd2' n = 
  case (even n) of
    True -> (n+2)
    False   -> n
nums x = 
  case compare x 0 of
    LT -> (-1)
    EQ -> 0
    GT -> 1

data Mood = Blah | Woot deriving (Show, Eq)

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood    _ = Blah

f1 :: (Num b, Ord a) => a ->b-> a
f1 = undefined

fn :: (a, b) -> (c, d) -> ((b, d), (a, c))
fn x y = (n, m)
  where m  = ((fst x), (fst y))
        n  = ((snd x), (snd y))

myAbs :: Integer -> Integer
myAbs x = if (x <= 0) then (negate x) else x 

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = if (x == (reverse x)) then True else False


sayHello ::String-> IO ()
sayHello x = putStrLn( "Hello, you rascal lkjlkj " ++ x ++ "!")

multi      = x * y
  where   x= 5
          y= 6


waxOn = x * 5
  where x = y ^ 2
        y = z + 8
        z = 7 

triple = \x -> x * 3

topLevelFunction :: Integer -> Integer
topLevelFunction = \x -> x + woot + topLevelValue
   where woot :: Integer
         woot = 10

topLevelValue :: Integer
topLevelValue = 5

area d = pi * (r * r)
  where r = d / 2

numInString :: String -> Char
numInString = \str -> (!!) str 3

strg = "Curry is Awesome"

numInString2 :: Int -> Char
numInString2 = \x -> (!!) strg x

data TisAnInteger =
   TisAn Integer

instance Eq TisAnInteger where
   (TisAn value1) ==  (TisAn value2) = 
      value1 == value2 

data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
   (Two a b) == (Two c d) =
     (a == c) && (b ==d) 

data StringOrInt =
    TisAnInt Int
  | TisAString String

instance Eq StringOrInt where
   (TisAnInt val1) == (TisAnInt val2) =
     val1 == val2  
   (TisAString c) == (TisAString d) =
       c == d
   _ == _ = False

data Pair a  =
   Pair a a 

instance Eq a => Eq (Pair a) where
   (Pair a b) == (Pair a' b') =
      (a == a') && (b == b') 
  

data Tuple a b =
   Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
   (Tuple a' b') == (Tuple a'' b'') =
     (a' == a'') && (b' == b'') 

data Which a =
   ThisOne a
 | ThatOne a

instance Eq a => Eq (Which a) where
   (ThisOne a') == (ThisOne b') =
     (a' == b')
   (ThatOne a'') == (ThatOne b'') =
     (a'' == b'')
   _ == _ = False

data EitherOr a b =
    Hello a
  | Goodbye b


instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (Hello a') == (Hello b') =
       (a' == b')
    ( Goodbye b'') == (Goodbye c'') =
       (b'' == c'')
    _ == _ = False

data DayOfWeek =
   Mon | Tue | Weds | Thu | Fri | Sat | Sun
   deriving (Eq, Ord, Show)

data Person = Person Bool
instance Show Person where
   show (Person True) = "True"
   show (Person False) = "False"

printPerson :: Person -> IO()
printPerson person = putStrLn (show person)
