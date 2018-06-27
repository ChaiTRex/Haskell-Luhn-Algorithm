{-# OPTIONS_GHC -dcore-lint -dcmm-lint -dstg-lint -Wall -O3 #-}
{-# LANGUAGE BangPatterns #-}

module Luhn (checkLuhn) where

import Data.Char  (digitToInt)
import Data.Bits  ((.&.))
import Data.Word  (Word8)

import Data.Array.Unboxed  ((!), UArray, array)

-- Quickly gets a list of digits from a nonnegative Integer
-- Gives error for negative inputs
-- Uses GMP's show for greatly-improved speed over GMP's div and mod
toDigits :: Integer -> [Word8]
{-# INLINE toDigits #-}
toDigits n = map (fromIntegral . digitToInt) . show $ n

-- Has the Luhn sums for each group of four digits from the right
digitSumArray :: UArray (Word8,Word8,Word8,Word8) Word8
{-# NOINLINE digitSumArray #-}
digitSumArray = let ds = [0..9]
                in  array ((0,0,0,0),(9,9,9,9)) [((a,b,c,d), digitDoublingArray!a + b + digitDoublingArray!c + d) | a <- ds, b <- ds, c <- ds, d <- ds]
  where
    -- Has the Luhn sums for digits that are doubled (8 doubled is 16; the digits 1 and 6 sum to 7)
    digitDoublingArray :: UArray Word8 Word8
    {-# NOINLINE digitDoublingArray #-}
    digitDoublingArray = array (0,9) [(0,0),(1,2),(2,4),(3,6),(4,8),(5,1),(6,3),(7,5),(8,7),(9,9)]

-- Has the number of zeroes to add onto the left to make the digit count a multiple of four
alignmentArray :: UArray Int Int
{-# NOINLINE alignmentArray #-}
alignmentArray = array (0,3) [(0,0),(1,3),(2,2),(3,1)]

-- Gets the Luhn sum, which is zero for valid inputs, of a list of digits
luhnSum :: [Word8] -> Word8
{-# INLINE luhnSum #-}
luhnSum xs = go 0 $ take (alignmentArray ! (length xs .&. 3)) [0,0,0] ++ xs
  where
    go :: Integer -> [Word8] -> Int
    {-# NOINLINE luhnSum' #-}
    go s (a:b:c:d:xs) = luhnSum' (s + toInteger (digitSumArray ! (a,b,c,d))) xs
    go s _            = fromInteger (s `rem` 10)

-- Checks whether a nonnegative Integer passes the Luhn algorithm
checkLuhn :: Integer -> Bool
{-# INLINABLE checkLuhn #-}
checkLuhn n = (n >= 0) && (luhnSum (toDigits n) == 0)
