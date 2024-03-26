module Sudoku where

import Data.Char

rows = "ABCD"
cols =  "1234"

containsElem :: Eq a => a ->  [a] -> Bool
containsElem _ [] = False
containsElem elem (x:xs)
  | elem == x = True
  | otherwise = containsElem elem xs

cross :: [a] -> [a] -> [[a]]
cross xs ys = [ [i, j] | i <- xs, j <- ys ]

replacePointsWithZeros :: String -> String
replacePointsWithZeros = map (\c -> if c == '.' then '0' else c)

squareStrings :: [String]
squareStrings = cross rows cols

squareStrings9x9 :: [String]
squareStrings9x9 = cross (rows ++ "EFGHI") (cols ++ "56789")


-- takes the String input and converts it into a sudoku board list
-- 1: replacePointsWithZeros in the input string.
-- 2: convert digits to Ints (map digitToInt)
-- 3: zip squareStrings with the resulting list
-- the result is a list of tuples, where the n:th square string 
-- is paired with the n:th digit in the input string.
parseBoard :: String -> [(String, Int)]
parseBoard str = zip squareStrings (map digitToInt (replacePointsWithZeros str))

-- a list of lists, where each list is composed of all squares in a row, column, or box in the board.
unitList :: [[String]]
unitList = 
  [ cross [r] cols | r <- rows ] ++
  [ cross rows [c] | c <- cols ] ++
  [ cross xs ys | xs <- ["AB", "CD"], ys <- ["12", "34"] ]

filterUnitList :: String -> [[String]]
filterUnitList sq = filter (containsElem sq) unitList

units :: [(String, [[String]])]
units = zip squareStrings (map filterUnitList squareStrings)

-- apply (++) to each element in the list from left to right, concatenating them to the empty list [].
foldList :: [[a]] -> [a]
foldList = foldr (++) []

-- base case: if the list is empty, the result is an empty list.
-- otherwise, we divide the list into its head and tail.
-- if the head is contained in the tail, discard it and use recursion on the tail.
-- otherwise, keep the head and continue recursion on the tail.
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs)
  | containsElem x xs = removeDuplicates xs
  | otherwise = x : removeDuplicates xs

-- zips two lists: the squareStrings list, and the peerList
-- the peerList contains the second elements of each pair in 'units', ie. peers incl. duplicates and the squares themselves
-- we then "flatten" the list (foldList), and remove duplicates. 
-- We still have to remove the squares themselves from the peerList, so finally we use a filter function.
peers :: [(String, [String])]
peers = (map removeSelf peerList)
  where peerList = zip squareStrings (map (removeDuplicates . foldList . snd) units)

removeSelf :: (String, [String]) -> (String, [String])
removeSelf (x, xs) = (x, filter (/= x) xs)

-- using list comprehension (better?)
peers2 :: [(String, [String])]
peers2 = [ (sq, filter (/= sq) peers) | (sq, peers) <- zip squareStrings (map (removeDuplicates . foldList . snd) units) ]