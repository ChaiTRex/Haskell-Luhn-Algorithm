{-# LANGUAGE BangPatterns #-}

module Luhn (checkLuhn) where

import Data.Char  (digitToInt)
import Data.Bits  (shiftL)

-- Uses GMP's show for greatly-improved speed over GMP's div and mod
toDigits :: Integer -> [Int]
toDigits n | n < 0     = []
           | otherwise = map digitToInt . show $ n

-- Quickly gets the digit sum of a nonnegative Int
digitSum :: Int -> Int
digitSum n = (n - 1) `rem` 9 + 1

-- Uses Data.Bits.shiftL to quickly double
luhnSum :: Integer -> [Int] -> Int
luhnSum !sum []              = fromInteger (sum `rem` 10)
luhnSum !sum [!x]            = fromInteger ((sum + toInteger x) `rem` 10)
luhnSum !sum !((!x):(!y):xs) = luhnSum (sum + toInteger x + toInteger (digitSum (shiftL y 1))) xs

checkLuhn :: Integer -> Bool
checkLuhn = (== 0) . luhnSum 0 . reverse . toDigits
