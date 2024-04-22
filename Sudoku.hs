-- Author: Emil Håkansson

module Sudoku where

import Data.Char
import System.Random

rows :: String
rows = "ABCD"

-- this way we only have to change the 'rows' variable to change the size of the board.
cols :: String
cols = (concat . map show) [1..(length rows)]

size :: Int
size = length rows

-- gets us the dimensions of each box as an integer (assuming 'size' is a square number)
boxSize :: Int
boxSize = (floor . sqrt . fromIntegral) size

-- helper function. splits a list/string at every n characters, then puts the substrings in a list.
-- used in order to calculate the boxes for the unitList function, depending on the boxSize.
-- example: 'split 3 "ABCDEFGHI"' returns ["ABC", "DEF", "GHI"]
splitN ::  Int -> [a] -> [[a]]
splitN n [] = []
splitN n xs
  | length xs <= n = [xs]
  | otherwise = take n xs : splitN n (drop n xs)

-- either ["AB", "CD"] for a 4x4 board, or ["ABC", "DEF", "GHI"] for a 9x9 board.
boxRows :: [String]
boxRows = splitN boxSize rows

-- either ["12", "34"] for a 4x4 board, or ["123", "456", "789"] for a 9x9 board.
boxCols :: [String]
boxCols = splitN boxSize cols

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
squareStrings :: [String]
squareStrings = cross rows cols

-- takes the String input and converts it into a sudoku board list
-- 1: replacePointsWithZeros in the input string.
-- 2: convert digits to Ints (map digitToInt)
-- 3: zip squareStrings with the resulting list
-- the result is a list of tuples, where the n:th square string 
-- is paired with the n:th digit in the input string.
parseBoard :: String -> [(String, Int)]
parseBoard str = zip squareStrings (map digitToInt (replacePointsWithZeros str))

-- same as parseBoard, but automatically calculates the board dimensions based on the string length,
-- instead of the constant 'rows', for example when reading from file. Works for sizes up to 9x9.
parseBoard2 :: String -> [(String, Int)]
parseBoard2 str = zip sqStrings (map digitToInt (replacePointsWithZeros str)) where
  sqStrings = cross rows_ cols_
  rows_ = take ((floor . sqrt . fromIntegral . length) str) "ABCDEFGHI"
  cols_ = (concat . map show) [1..(length rows_)]

-- a list of lists, where each list is composed of all squares in a row, column, or box in the board.
unitList :: [[String]]
unitList = 
  [ cross [r] cols | r <- rows ] ++
  [ cross rows [c] | c <- cols ] ++
  [ cross xs ys | xs <- boxRows, ys <- boxCols ]

-- retrieves the unit list for a given square string
-- the unit list is a list of the row, column and box that the square belongs to.
-- each row, column or box is itself a list of strings.
filterUnitList :: String -> [[String]]
filterUnitList sq = filter (containsElem sq) unitList

-- every unit list of every square string, contained as pairs in a list.
-- A row, column, or box contains the squares in that unit, represented by square strings.
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

-- takes as input the list of units, i.e. every pair of square strings and its unit list
-- for every unit list, it folds ("flattens") that list, removes duplicates, then filters any string in the unit list
-- that is the same as the square itself.
-- the result is a list of tuples, where the first element of the pair is a square, and the second is a list of its peers.
peers :: [(String, [String])]
peers = map (\(sq, p) -> (sq, (filter (/= sq) . removeDuplicates . foldList) p)) units

-- Lab 2:

fromMaybe :: a -> Maybe a -> a
fromMaybe defaultVal maybeVal =
  case maybeVal of
    Just val -> val
    Nothing -> defaultVal

-- if the string is not contained in peers, return an empty list.
-- otherwise, we simply use lookup to find sq's corresponding peers.
getPeers :: String -> [String]
getPeers sq = fromMaybe [] (lookup sq peers)

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

-- to verify a sudoku string, we simply parse the string into a board and apply 
-- the validBoardNumbers and validUnits functions.
verifySudoku :: String -> Bool
verifySudoku = validUnits . validBoardNumbers . parseBoard2

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

sudoku9x9 :: String
sudoku9x9 = "000123000456000000000000789000000000000000000000000000000000000000000000000000000"

-- filters the elements contained in ys from xs, i.e. return xs with all elements from ys removed.
reduceList :: Eq a => [a] -> [a] -> [a]
reduceList xs ys = filter (\x -> not (elem x ys)) xs

-- if the square is not filled, we use the same idea as before:
-- use lookups to get the values of every peer square.
-- then, we use reduceList with the full list of valid numbers (1..4 for 4x4),
-- to remove those peer values. Finally, return a tuple with the square and the resulting list.
validSquareNumbers :: (String, Int) -> [(String, Int)] -> (String, [Int])
validSquareNumbers (sq, n) board
  | n == 0 = (sq, reduceList [1..size] (lookups (getPeers sq) board))
  | otherwise = (sq, [n])

-- maps the validSquareNumbers function to every square on the board.
-- the result is a list of tuples, where every tuple contains the square string, and a list of its valid numbers.
validBoardNumbers :: [(String, Int)] -> [(String, [Int])]
validBoardNumbers board = map (\(sq, n) -> validSquareNumbers (sq, n) board) board

-- helper function, checks if there are any elements xs in the validBoardNumbers for 
-- the unit which are of length 1 (i.e. there is only one valid number for that square) and 
-- which occur more than once (i.e. there are two squares conflicting over this number)
containsNoSingleDuplicates :: [String] ->  [(String, [Int])] -> Bool
containsNoSingleDuplicates unit board = (length . removeDuplicates) xss == length xss where
  xss = filter (\xs -> length xs == 1) (lookups unit board)

-- we can check if there is a possibility to insert every number in at least one square,
-- by concatenating the validBoardNumbers and checking if every number ([1..4] / [1..9]) is contained in that list.
-- We also check that there are no single-element duplicate lists, i.e. direct conflicts.
validUnit :: [String] -> [(String, [Int])] -> Bool
validUnit unit board = containsNoSingleDuplicates unit board && all (\x -> elem x (concat (lookups unit board))) [1..size]

-- to check if every unit is valid, we can simply apply the validUnit function to every unit in the unitList,
-- and return true iff. all units are valid for a given board.
validUnits :: [(String, [Int])] -> Bool
validUnits board = all (\x -> validUnit x board) unitList

-- Lab 3

giveMeANumber :: IO ()
giveMeANumber = do 
  lowerStr <- getLine
  upperStr <- getLine
  let lower = read lowerStr :: Int
  let upper = read upperStr :: Int
  rnd <- randomRIO (lower, upper)
  putStrLn (show rnd)

-- helper function. returns the number of elements 'x' in xs.
count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

-- helper function. takes a unit of (square, val)-tuples as input, returns the squares that have direct conflicts (i.e. the same val)
-- 0 is considered an empty square, and so is omitted.
unitConflicts :: [(String, Int)] -> [String]
unitConflicts unit = map fst (filter (\(sq, val) -> (count val (map snd unit)) > 1 && val /= 0) unit)

-- helper function. gets the (square, val) pairs in a given unit from a given board, essentially
-- a "lookups" for tuples.
filterUnit :: [String] -> [(String, Int)] -> [(String, Int)]
filterUnit unit board = filter (\(sq, val) -> sq `elem` unit) board

-- helper function. for a given board, assembles a list of lists, where every sub-list is a collection of
-- (square, val) tuples, representing a unit.
unitVals :: [(String, Int)] -> [[(String, Int)]]
unitVals board = map (\u -> (filterUnit u board)) unitList

-- finally, we apply the unitConflicts function over every sub-list in unitVals.
-- concatenating this gives us a list of every conflicting square on the board.
boardConflicts :: [(String, Int)] -> [String]
boardConflicts board = concat (map unitConflicts (unitVals board))

printSudoku :: [(String, Int)] -> IO ()
printSudoku board = do
  let size =  (floor . sqrt . fromIntegral . length) board
  let boxSize = (floor . sqrt . fromIntegral) size
  let vals = map snd board
  let rows = map (concatMap show) (splitN size vals)

  let validNbrs = validBoardNumbers board
  let blockings = [sq | (sq, vals) <- filter (\(sq, vals) -> vals == []) validNbrs] 

  let conflicts = boardConflicts board

  putStrLn "---------"
  mapM_ putStrLn rows
  if blockings == [] then
    putStrLn "Blocked squares: None" else putStrLn ("Blocked squares: " ++ (show blockings))
  if conflicts == [] then
    putStrLn "Conflicting squares: None" else putStrLn ("Conflicting squares: " ++ (show conflicts))

splitString :: Char -> String -> [String]
splitString sep [] = [""]
splitString sep (x:xs)
  | x == sep = "" : (splitString sep xs)
  | otherwise = (x : head (splitString sep xs)) : tail (splitString sep xs)

main :: IO ()
main = do
  file <- readFile "easy50.txt"
  let raw = (filter (/= '\n') file)
  let list = filter (/= "") (splitString '=' raw)

  let size = (floor . sqrt . fromIntegral . length . head) list
  let rows_ = take size "ABCDEFGHI"
  let cols_ = (concat . map show) [1..size]
  let boxSize_ = (floor . sqrt . fromIntegral) size
  let boxRows_ = splitN boxSize_ rows_
  let boxCols_ = splitN boxSize_ cols_
  let unitList_ = [ cross [r] cols_ | r <- rows_ ] ++ [ cross rows_ [c] | c <- cols_ ] ++ [ cross xs ys | xs <- boxRows_, ys <- boxCols_ ]
  let filterUnitList sq = filter (containsElem sq) unitList


  let verified = map verifySudoku list

  mapM_ (\b -> printSudoku b) (map parseBoard2 list)