module WordNumber ( myWords

                    ) where

import Data.List (intersperse)

-- can use only dropWhile and takeWhile
-- takeWhile (/=' ') "all something" will take everything up to
-- the space
-- dropWhile (==' ') " all something" will take "all something"
myWords :: String -> [String]
myWords str = myWords' str [] where
  myWords' str accum = case str of
    []         -> accum
    x@(' ': xs)  -> myWords' [] accum ++ [dropWhile(==' ') x]
    xs         -> let (a, b) = (takeWhile (/= ' ') xs, dropWhile(/= ' ') xs)
                  in myWords' b (accum ++ [a])

dropChars :: (a -> Bool) -> [a] -> [a]
dropChars f = dropWhile f
takeChars :: (a -> Bool) -> [a] -> [a]
takeChars f = takeWhile f

some' :: (Enum a, Ord a) => a -> a -> [a] -> [a]
some' st end accum =
     let end'     = end
         accum'= end' : accum
         next  = pred end'
     in  if (end' == st) then accum' else some' st next accum'


myEnumFromTo :: (Enum a, Ord a) => a -> a-> [a]
myEnumFromTo start end  = some' start end []


digitToWord :: Int -> String
digitToWord n = concat $ map wordNumber' $ intersperse "-" $ map wordNumber $ digits n
--  let m =digits' n []
--  in concat $ map wordNumber' $ intersperse "-" $ map wordNumber m


digits' :: Int -> [Int] -> [Int]
digits' 0 accum = accum
digits' n accum =
  let lastval' = n `mod` 10
      nextval' = n `div` 10
      nextround = concat [digits' nextval' [lastval'] ++ accum]
  in nextround


digits :: Int -> [Int]
digits  = flip digits' []

wordNumber :: Int -> String
wordNumber n = case n of
   1 -> "one"
   2 -> "two"
   3 -> "three"
   4 -> "four"
   5 -> "five"
   6 -> "six"
   7 -> "seven"
   8 -> "eight"
   9 -> "nine"
   _ -> undefined

wordNumber' :: String -> String
wordNumber' val = case val of
   "-" -> "-"
   _   -> val



-- concat $ map wordNumber' $ intersperse "-" $ map wordNumber m
