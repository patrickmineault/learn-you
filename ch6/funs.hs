compareWithHundred :: (Num a, Ord a) => a -> Ordering  
compareWithHundred = compare 100

addOne :: (Num a) => a -> a
addOne = (+1)

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isAlphaNumeric :: Char -> Bool
isAlphaNumeric = (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'])

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys  

cumsum :: (Num a) => [a] -> [a]
cumsum a = scanl1 (+) a