-- Author: Emil Håkansson

module Sudoku where

import Data.Char

-- How to run:
-- 1. Change the constant "testFile" to change which file should be verified. (easy50.txt, inconsistent20.txt, conflicts.txt, blockings.txt)
-- 2. run 'ghci'
-- 3. Load the module: ':load VerifySudoku.hs'
-- 4. run the main function: 'main'

-- change this to change which file to verify.
testFile :: String
testFile = "easy50.txt"


-- helper function. splits a string at every n characters, then puts the substrings in a list.
-- used in order to calculate the boxes for the unitList function, depending on the boxSize.
-- example: 'split 3 "ABCDEFGHI"' returns ["ABC", "DEF", "GHI"]
splitEvery ::  Int -> String -> [String]
splitEvery n [] = []
splitEvery n str
  | length str <= n = [str]
  | otherwise = take n str : splitEvery n (drop n str)

containsElem :: Eq a => a ->  [a] -> Bool
containsElem _ [] = False
containsElem e (x:xs)
  | e == x = True
  | otherwise = containsElem e xs

-- returns a list of every pair x and y,
-- where x is an element of xs and y is an element of ys
cross :: [a] -> [a] -> [[a]]
cross xs ys = [ [i, j] | i <- xs, j <- ys ]

replacePointsWithZeros :: String -> String
replacePointsWithZeros = map (\c -> if c == '.' then '0' else c)

-- a list of every square in the sudoku board, represented as strings
squareStrings :: Int -> [String]
squareStrings size = cross rows_ cols_ where
    rows_ = take size "ABCDEFGHI"
    cols_ = take size "123456789"

-- takes the String input and converts it into a sudoku board list
-- 1: replacePointsWithZeros in the input string.
-- 2: convert digits to Ints (map digitToInt)
-- 3: zip sqStrings with the resulting list
-- the result is a list of tuples, where the n:th square string is paired with the n:th digit in the input string.
-- works for variable sizes of sudoku boards, up to 9x9.
parseBoard :: String -> [(String, Int)]
parseBoard str = zip sqStrings (map digitToInt (replacePointsWithZeros str)) where
  sqStrings = cross rows_ cols_
  rows_ = take ((floor . sqrt . fromIntegral . length) str) "ABCDEFGHI"
  cols_ = (concat . map show) [1..(length rows_)]

-- a list of lists, where each list is composed of all squares in a row, column, or box in the board.
unitList :: Int -> [[String]]
unitList size = 
  [ cross [r] cols | r <- rows ] ++
  [ cross rows [c] | c <- cols ] ++
  [ cross xs ys | xs <- boxRows, ys <- boxCols ] where
    rows = take size "ABCDEFGHI"
    cols = take size "123456789"
    boxRows = splitEvery boxSize rows
    boxCols = splitEvery boxSize cols
    boxSize = (floor . sqrt . fromIntegral) size

-- retrieves the unit list for a given square string
-- the unit list is a list of the row, column and box that the square belongs to.
-- each row, column or box is itself a list of strings.
filterUnitList :: Int -> String -> [[String]]
filterUnitList size sq = filter (containsElem sq) (unitList size)

-- every unit list of every square string, contained as pairs in a list.
-- A row, column, or box contains the squares in that unit, represented by square strings.
units :: Int -> [(String, [[String]])]
units size = zip (squareStrings size) (map (\sq -> filterUnitList size sq) (squareStrings size))

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

-- takes as input an Int representing the dimensions of the board, then calculates the list of units, 
-- i.e. every pair of square strings and its unit list.
-- for every unit list, it folds ("flattens") that list, removes duplicates, then filters any string in the unit list
-- that is the same as the square itself.
-- the result is a list of tuples, where the first element of the tuple is a square, and the second is a list of its peers.
peers :: Int -> [(String, [String])]
peers size = map (\(sq, p) -> (sq, (filter (/= sq) . removeDuplicates . foldList) p)) (units size)

-- Lab 2:

fromMaybe :: a -> Maybe a -> a
fromMaybe defaultVal maybeVal =
  case maybeVal of
    Just val -> val
    Nothing -> defaultVal

-- if the string is not contained in peers, return an empty list.
-- otherwise, we simply use lookup to find sq's corresponding peers.
getPeers :: Int -> String -> [String]
getPeers size sq = fromMaybe [] (lookup sq (peers size))

-- using recursion:
-- if the list is empty, return an empty list.
-- if the head is Just x, add x back and justify the tail.
-- if the head is Nothing, discard it and justify the tail.
justifyList :: [Maybe a] -> [a]
justifyList [] = []
justifyList (Just x:xs) = x : justifyList xs
justifyList (Nothing:xs) = justifyList xs

-- we take the input list, xs, and map "lookup x ys" on every element x in that list.
-- the result is a list of Just- and Nothing-values, so we apply justifyList to the result to 
-- extract only the values themselves.
lookups :: Eq a => [a] -> [(a, b)] -> [b]
lookups xs ys = justifyList (map (\x -> lookup x ys) xs)

consistentSudoku :: String
consistentSudoku   = ".1....2.1.3...1."
-- .1..
-- ..2.
-- 1.3.
-- ..1.
inconsistentSudoku :: String
inconsistentSudoku = "1....2343.211.3."
-- 1...
-- .234
-- 3.21
-- 1.3.

inconsistentSudoku2 :: String
inconsistentSudoku2 = "0120021030004000"
-- 0120
-- 0210
-- 3000
-- 4000

inconsistentSudoku3 :: String
inconsistentSudoku3 = "1234000030420100"
-- 1234
-- 0000
-- 3042
-- 0100

-- filters the elements contained in ys from xs, i.e. return xs with all elements from ys removed.
reduceList :: Eq a => [a] -> [a] -> [a]
reduceList xs ys = filter (\x -> not (elem x ys)) xs

-- if the square is not filled, we use the same idea as before:
-- use lookups to get the values of every peer square.
-- then, we use reduceList with the full list of valid numbers (1..4 for 4x4),
-- to remove those peer values. Finally, return a tuple with the square and the resulting list.
validSquareNumbers :: (String, Int) -> [(String, Int)] -> (String, [Int])
validSquareNumbers (sq, n) board
  | n == 0 = (sq, reduceList [1..size] (lookups (getPeers size sq) board))
  | otherwise = (sq, [n]) where
    size = (floor . sqrt . fromIntegral . length) board

-- maps the validSquareNumbers function to every square on the board.
-- the result is a list of tuples, where every tuple contains the square string, and a list of its valid numbers.
validBoardNumbers :: [(String, Int)] -> [(String, [Int])]
validBoardNumbers board = map (\(sq, n) -> validSquareNumbers (sq, n) board) board

-- helper function. checks if there are any elements xs in the validBoardNumbers for 
-- the unit which are of length 1 (i.e. there is only one valid number for that square) and 
-- which occur more than once (i.e. there are two squares conflicting over this number)
containsNoSingleDuplicates :: [String] ->  [(String, [Int])] -> Bool
containsNoSingleDuplicates unit board = (length . removeDuplicates) xss == length xss where
  xss = filter (\xs -> length xs == 1) (lookups unit board)

-- we can check if there is a possibility to insert every number in at least one square,
-- by concatenating the validBoardNumbers and checking if every number [1..size] is contained in that list.
-- We also check that there are no single-element duplicate lists, i.e. direct conflicts.
validUnit :: [String] -> [(String, [Int])] -> Bool
validUnit unit board = containsNoSingleDuplicates unit board && all (\x -> elem x (concat (lookups unit board))) [1..size] where
    size = (floor . sqrt . fromIntegral . length) board

-- to check if every unit is valid, we can simply apply the validUnit function to every unit in the unitList,
-- and return true iff. all units are valid for a given board.
validUnits :: [(String, [Int])] -> Bool
validUnits board = all (\x -> validUnit x board) (unitList size) where
    size = (floor . sqrt . fromIntegral . length) board

-- to verify a sudoku string, we simply parse the string into a board and apply 
-- the validBoardNumbers and validUnits functions.
verifySudoku :: String -> Bool
verifySudoku = validUnits . validBoardNumbers . parseBoard

-- helper function to parse sudokus from file input. 
-- splits a string at a given Char separator and returns a list of every String
-- separated by that Char.
splitString :: Char -> String -> [String]
splitString sep [] = [""]
splitString sep (x:xs)
  | x == sep = "" : (splitString sep xs)
  | otherwise = (x : head (splitString sep xs)) : tail (splitString sep xs)

main :: IO ()
main = do
    file <- readFile testFile
    let raw = (filter (/= '\n') file)
        sudokuList = filter (/= "") (splitString '=' raw)

        verified = map (show . verifySudoku) sudokuList

    mapM_ putStrLn verified