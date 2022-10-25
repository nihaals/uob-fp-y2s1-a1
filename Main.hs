-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}
-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module PracticeTest (checksum, golfScorer, highlyDivisible, largestOddFactor, equals, babylonianPalindromes) where

import Types

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

{- Question 1 -}
checksum :: Integral a => [a] -> Bool
checksum a = (length a == 8) && (sum a `mod` 11 == 0)

{- Question 2 -}
golfScorer :: Integer -> Integer -> Integer
golfScorer par strokes
  | strokes == 1 = 5 -- Hole-in-one
  | strokes <= par - 2 = 4 -- Eagle
  | strokes == par - 1 = 3 -- Birdie
  | strokes == par = 2 -- Par
  | strokes == par + 1 = 1 -- Bogey
  | otherwise = 0

{- Question 3 -}
isHighlyDivisible :: Int -> Bool
isHighlyDivisible n = all (\x -> n `mod` x == 0) [2 .. 12]

highlyDivisible :: Int -> [Int]
highlyDivisible n = [x | x <- [1 .. n], isHighlyDivisible x]

getLargestOddFactor :: Int -> Int
getLargestOddFactor n = maximum [x | x <- [1 .. n], odd x, n `mod` x == 0]

largestOddFactor :: Int -> [Int]
largestOddFactor n = [getLargestOddFactor x | x <- [1 .. n]]

{- Question 4 -}
equals :: (Enum a, Bounded a, Eq b) => (a -> b) -> (a -> b) -> Bool
equals f g = all (\x -> f x == g x) [minBound .. maxBound]

{- Question 5 -}
foo :: (Integer, [Integer]) -> (Integer, [Integer])
foo (0, x) = (0, x)
foo (n, x) = foo (n `div` 60, x ++ [n `mod` 60])

toBase60 :: Integer -> [Integer]
toBase60 n = do
  let (_, out) = foo (n, [])
  out

isPalindrome :: Integer -> Bool
isPalindrome n = toBase60 n == reverse (toBase60 n)

babylonianPalindromes :: [Integer]
babylonianPalindromes = [x | x <- [1 ..], isPalindrome x]
