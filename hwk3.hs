-- Seymone Gugneja
-- CSCI 3675, Homework 3: Haskell
-- October 2nd, 2022

import Data.Char
main :: IO()

-- 1: Write a recursive function squareOddsRecursive that returns the square of each odd number in the list
squareOddsRecursive :: [Int] -> [Int]
squareOddsRecursive [] = []
squareOddsRecursive (x:xs)
    | odd x = x^2 : z
    | otherwise = z
    where z = squareOddsRecursive xs

-- res = squareOddsRecursive [0, 2, 1, 7, 8, 56, 17, 18]
-- main = print res

-- 2: Use list comprehension to solve problem 1
squareOddsComprehension :: [Int] -> [Int]
squareOddsComprehension xs = [x*x | x <- xs, odd x]

-- res = squareOddsComprehension [0, 2, 1, 7, 8, 56, 17, 18]
-- main = print res

-- 3: Write a function capitalizeComprehension, which, given a word, will capitalize it
capitalizeComprehension :: [Char] -> [Char]
capitalizeComprehension (c:cs) = toUpper c :[toLower d | d <- cs]

-- res = capitalizeComprehension "grEEnViLLe"
-- main = print res

-- 4: Recursively find the product of numbers in a list

prodRecursive :: (Num a) => [a] -> a
prodRecursive [] = 1
prodRecursive (x:xs) = x * prodRecursive xs

-- res1 = prodRecursive [3,2,1]
-- res2 = prodRecursive [3.0,2.0,1.0]
-- main = print res1
-- main = print res2

-- 5: Recursively check if a string is a palindrome
palindromeRecursive :: String -> Bool
palindromeRecursive [] = True
palindromeRecursive [_] = True
palindromeRecursive xs = (head xs == last xs) && palindromeRecursive (tail(init xs))

-- res = palindromeRecursive "abbc"
-- main = print res

-- 6: Recursively return all numbers in the input list within the range of two given numbers
inRangeRecursive :: Int -> Int -> [Int] -> [Int]
inRangeRecursive _ _ [] = []
inRangeRecursive h l (x:xs)
  | h <= x && x <= l  = x : z
  | otherwise = z
  where z = inRangeRecursive h l xs

-- res = inRangeRecursive 5 10 [9,3,12]
-- main = print res

-- 7: Use list comprehension to return all numbers in the input list within the range of two given numbers
inRangeComprehension :: Int -> Int -> [Int] -> [Int]
inRangeComprehension h l xs = [x | x <-xs, x >= h && x <= l]

-- res = inRangeComprehension 5 20 [9,3,12,1,7,14,22]
-- main = print res

-- 8: Recursively implement the power function
-- Side note: All of my other questions work in both iHaskell and GHC, but when I tested this one, it only worked in iHaskell
power :: (Num a, Integral b) => a -> b -> a
power _ 0 = 1
power a b = a * power a (b-1)

-- res1 = power 2.0 1
-- res2 = power 2 3
-- main = res1
