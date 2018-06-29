{-# OPTIONS_GHC -dcore-lint -dcmm-lint -dstg-lint -Wall -O3 -fobject-code #-}
{-# LANGUAGE BangPatterns, MagicHash, TemplateHaskell, UnboxedTuples #-}

-- WARNING: If loading in GHCi gives a panic message, either:
--   * start ghci without code loaded:  ghci -fobject-code
--   * start ghci with code loaded:     ghci -fobject-code Luhn
module Luhn  (checkLuhn, luhnDigitToAppend) where

import GHC.Exts     (Int#, Int(I#), (+#), (-#), (==#), (/=#), (>=#), andI#, inline, negateInt#, quotRemInt#, uncheckedIShiftL#)
import GHC.Integer  (integerToInt, leInteger, quotRemInteger)

import Language.Haskell.TH.Syntax  (Exp(LitE), Lit(IntegerL), returnQ)

maxIntAsInteger :: Integer
{-# NOINLINE maxIntAsInteger #-}
maxIntAsInteger = $( returnQ . LitE . IntegerL . toInteger $ (maxBound :: Int) )

maxIntPowerOf100AsInteger :: Integer
{-# NOINLINE maxIntPowerOf100AsInteger #-}
maxIntPowerOf100AsInteger = $( returnQ . LitE . IntegerL . product . map (const (100 :: Integer)) . takeWhile (>= 100) . iterate (flip div 100) $ (maxBound :: Int) )

-- Gets the Luhn sum, which is zero for valid inputs, of a nonnegative Integer
-- Negative inputs produce erroneous outputs
luhnSumInteger :: Integer -> Int
{-# INLINE luhnSumInteger #-}
luhnSumInteger = \ x -> let !y = digitGroupLoop 0# x
                            !r = I# y
                        in  r
  where
    digitGroupLoop :: Int# -> Integer -> Int#
    digitGroupLoop = \ !s !x -> let !fitsInInt = leInteger x maxIntAsInteger
                                in  case fitsInInt of
                                         False -> let !(# !x', !y' #) = quotRemInteger x maxIntPowerOf100AsInteger  -- get next several pairs of digits
                                                      !y              = integerToInt y'
                                                      !s'             = digitPairLoop s y
                                                  in  digitGroupLoop s' x'
                                         _     -> let !y  = integerToInt x
                                                      !s' = digitPairLoop s y
                                                  in  s'
    
    digitPairLoop :: Int# -> Int# -> Int#
    digitPairLoop = \ !s !x -> let !noPairsLeft = x ==# 0#
                               in  case noPairsLeft of
                                        0# -> let !(# !x', !y #) = quotRemInt# x 100#  -- get next pair of digits
                                                  !(# !a', !b #) = quotRemInt# y  10#  -- get each digit from pair
                                                                 {-2 * a'-}
                                                  !a             = uncheckedIShiftL# a' 1#
                                                                 {-if a >= 10 then a - 9 + b + s else a - 0 + b + s-}
                                                  !s'            = a -# (andI# 9# (negateInt# (a >=# 10#))) +# b +# s
                                                                 {-if s' >= 20 then s' - 20 else if s' >= 10 then s' - 10 else s' - 0-}
                                                  !s''           = s' -# (andI# (uncheckedIShiftL# 10# (s' >=# 20#)) (negateInt# (s' >=# 10#)))
                                              in  digitPairLoop s'' x'
                                        _  -> s

-- Checks whether a nonnegative Integer passes the Luhn algorithm
-- Negative inputs produce False
checkLuhn :: Integer -> Bool
{-# INLINABLE checkLuhn #-}
checkLuhn = \ n -> (n >= 0) && (inline luhnSumInteger n == 0)

-- Returns the digit to append to a nonnegative Integer so that the result passes the Luhn algorithm
-- Negative inputs produce Nothing
luhnDigitToAppend :: Integer -> Maybe Int
{-# INLINABLE luhnDigitToAppend #-}
luhnDigitToAppend = \ n -> if n < 0 then Nothing else Just . inline go . inline luhnSumInteger . (*10) $ n
  where                         {-if x /= 0 then 10 - x else 0-}
    go = \ (!(I# !x)) -> let !y = andI# (10# -# x) (negateInt# (x /=# 0#))
                             !r = I# y
                         in  r

