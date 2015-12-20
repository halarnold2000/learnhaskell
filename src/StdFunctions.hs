module StdFunctions (
                      myOr
                    , myAny
                    , myAny'
                    , myElem
                    , myElemAny
                    )where





--myElemAny :: Eq a => a -> [a] -> Bool
myElemAny a = any (== True) . map (== a) 


myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (x : xs) = if a == x then True else myElem a xs

myOr :: [Bool] -> Bool
myOr (x : xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny  fn = myOr . myAny' fn

myAny' :: (a -> Bool) -> [a] -> [Bool]
myAny' _ [] = []
myAny' fn ( x : xs) = fn x : myAny' fn xs
