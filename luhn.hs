lastDigit :: Integer -> Integer
dropLastDigit :: Integer -> Integer

lastDigit 0 = 0
lastDigit n = n `mod` 10

dropLastDigit 0 = 0
dropLastDigit n = (n - (lastDigit n)) `div` 10


toRevDigits :: Integer -> [Integer]
toRevDigits 0 = []
toRevDigits n 
    | n < 0 = []
toRevDigits n = lastDigit n:(toRevDigits (dropLastDigit n))

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:[]) = [x,y*2]
doubleEveryOther (x:y:xs) = x:y*2:doubleEveryOther xs

sumDigits :: [Integer] -> Integer
sumDigits [x] = x
sumDigits (x:xs) = lastDigit x + (sumDigits [dropLastDigit x]) + sumDigits xs

luhn :: Integer -> Bool
luhn x = sumDigits (doubleEveryOther (toRevDigits x)) `mod` 10 == 0 
