module Reverse 
   ( rvrs
   ) where

util :: Int -> Int-> String -> String
util x y s = drop x $ take y s

rvrs :: String -> String
rvrs  x  = three ++ two ++ one 
  where one, two, three :: String 
        one = util 0 5 x
        two  = (++) (util  6 8 x)  " "
        three = (++) (util 9 16 x) " " 


--sayHello ::String-> IO ()
--sayHello x = putStrLn( "Hello, you rascal " ++ x ++ "!")

