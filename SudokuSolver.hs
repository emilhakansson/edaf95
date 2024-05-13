-- Interactive Sudoku solver
-- Author: Emil Håkansson, Felicia Huynh

-- How to use:
-- 1. Open ghci
-- 2. Load the module: ':load SudokuSolver.hs'
-- 3. Run the main function: 'main'
-- 4. Type the name of the text file to solve. For example, 'easy50.txt'.
-- 5a. Type a number to choose the corresponding option on screen.
-- 5b. Follow the instructions on screen. 
-- 5c. In order to solve a sudoku interactively, type 3. Then enter a square and a value in the correct format.
-- For example, type 'A1 4' to assign a 4 to the square A1. The program will give a warning if the assignment 
-- results in an invalid sudoku. Otherwise, it prints the resulting board and allows the user to continue.
-- Type 'solve' to automatically solve the current sudoku and return to the list of options.

module SudokuSolver where

-- by Adrian Roth

import Data.Bool
import Data.Char
import Data.List
import Data.Maybe
import Control.Concurrent

splitEvery ::  Int -> [a] -> [[a]]
splitEvery n [] = []
splitEvery n xs
  | length xs <= n = [xs]
  | otherwise = take n xs : splitEvery n (drop n xs)

cross :: [a] -> [a] -> [[a]]
cross s1 s2 = [[r, c] | r <- s1, c <- s2]

rowBoxes, colBoxes :: [String]
rowBoxes = ["ABC", "DEF", "GHI"]
colBoxes = ["123", "456", "789"]

rows, cols :: String
rows = concat rowBoxes
cols = concat colBoxes

squares :: [String]
squares = cross rows cols

unitlist :: [[String]]
unitlist = [cross rows [c] | c <- cols]
        ++ [cross [r] cols | r <- rows]
        ++ [cross rs cs | rs <- rowBoxes, cs <- colBoxes]

units :: [(String, [[String]])]
units = [(s, filter (elem s) unitlist) | s <- squares]

peers :: [(String, [String])]
peers = map (\(s, u) -> (s, delete s (foldl union [] u))) units

type Board = [(String, [Int])]
allDigits :: [Int]
allDigits = [1, 2, 3, 4, 5, 6, 7, 8, 9]
infAllDigits = repeat allDigits
emptyBoard = zip squares infAllDigits

parseSquare :: (String, Char) -> Board -> Maybe Board
parseSquare (s, x) values
  | x == '.' || x == '0' = return values
  | isDigit x = assign (digitToInt x) s values
  | otherwise = fail "not a valid grid"

parseBoard :: String -> Maybe Board
parseBoard = foldr ((=<<) . parseSquare) (Just emptyBoard) . zip squares

map2 :: (a -> c, b -> d) -> (a, b) -> (c, d)
map2 (f1,f2) (a,b) = (f1 a, f2 b)

mapIf :: (a -> a) -> (a -> Bool) -> [a] -> [a]
mapIf _ _ [] = []
mapIf f pred (x:xs)
  | pred x    = f x : mapIf f pred xs
  | otherwise = x : mapIf f pred xs

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (x:xs)
  | isJust x  = x
  | otherwise = firstJust xs

lookupList :: Eq a => a -> [(a, [b])] -> [b]
lookupList _ [] = []
lookupList y ((a, bList): rest)
  | y == a    = bList
  | otherwise = lookupList y rest

-- Sets the valid numbers for a given square to a single element list containing only the given value.
-- Returns a board with the valid numbers set to val.
setValue :: Int -> String -> Board -> Board
setValue val sq = mapIf (\(square, vals) -> (square, [val])) (\(square, vals) -> square == sq)

-- Takes a value and square, and removes the value from that square's list of valid numbers.
-- Returns a Board with the value removed from the given square's valid numbers.
-- map2 maps id to sq, and filter (/= val) to the number list, leaving sq unchanged and removing val from the list of values.
-- mapIf maps this pair of functions to the board, only applying if square == sq.
eliminateValue :: Int -> String -> Board -> Board
eliminateValue val sq = mapIf (map2 (id, filter (/= val))) (\(square, vals) -> square == sq)

-- Works similar to eliminateValue, but has the return type Maybe Board.
-- Returns Nothing if the elimination would result in an invalid board. 
-- Otherwise, it returns Just board with the given value eliminated.
eliminate :: Int -> String -> Board -> Maybe Board
eliminate val sq board
  | vals == [val] = Nothing
  | null vals     = Nothing
  | otherwise     = Just (eliminateValue val sq board) where
    vals = lookupList sq board

-- Assigns a square in the board a given value, and eliminates that value from its peers.
-- Returns Nothing if the assignment results in an invalid board, otherwise returns Just board with the given assignment.
-- With bind, this can be chained to fill any number of squares. This fills the first box with numbers 1-9:
--  assign 1 "A1" emptyBoard >>= assign 2 "A2" >>= assign 3 "A3" >>= 
--  assign 4 "B1" >>= assign 5 "B2" >>= assign 6 "B3" >>= 
--  assign 7 "C1" >>= assign 8 "C2" >>= assign 9 "C3"
assign :: Int -> String -> Board -> Maybe Board
assign val sq board = assign' val peerList (setValue val sq board) where
  peerList = lookupList sq peers

-- Helper function that takes 3 parameters: a value to eliminate, the peer list of a square, and a board.
-- Recursively eliminates the given value for each square in the peer list.
-- Returns "Just board" with the given value eliminated from each square in the peer list. 
-- Returns Nothing if the elimination would fail.
assign' :: Int -> [String] -> Board -> Maybe Board
assign' _ _ [] = Nothing -- If no valid board, return Nothing
assign' val [] board = Just board  -- Base case: if peerList is empty, return Just board
assign' val (p:peerList) board = eliminate val p board >>= assign' val peerList

solveSudoku' :: [String] -> Board -> Maybe Board
solveSudoku' [] board = Just board
solveSudoku' (sq:sqs) board = firstJust (map (\v -> assign v sq board >>= (solveSudoku' sqs)) (lookupList sq board))

solveSudoku :: String -> Maybe Board
solveSudoku str = case parseBoard str of
  Nothing -> Nothing 
  Just board -> solveSudoku' squares board

splitString :: Char -> String -> [String]
splitString sep [] = [""]
splitString sep (x:xs)
  | x == sep = "" : splitString sep xs
  | otherwise = (x : head (splitString sep xs)) : tail (splitString sep xs)

parseFile :: String -> IO [String]
parseFile contents = do
  return $ (filter (/= "") . splitString '=' . filter (/= '\n')) contents

boardString :: Board -> String
boardString = concatMap show . concatMap snd

printSudoku :: String -> IO ()
printSudoku str = do
  let sep = "==========="
      delim = "|"
      rowIndex = unwords rowBoxes
      rows = splitEvery 9 str
      prettyRows = map (intercalate delim . splitEvery 3) rows
  putStrLn rowIndex
  mapM_ putStrLn prettyRows
  putStrLn sep

printBoard :: Maybe Board -> IO ()
printBoard board = case board of
  Nothing -> putStrLn "Invalid sudoku! No solution possible."
  Just b -> do
    putStrLn "Solution found: "
    printSudoku $ boardString b

colIndices :: [(String, Int)]
colIndices = [("A", 0), ("B", 1), ("C", 2), ("D", 3), ("E", 4), ("F", 5), ("G", 6), ("H", 7), ("I", 8)]

lookupCol :: String -> Maybe Int
lookupCol sq = lookup [head sq] colIndices
  
getPos :: String -> Maybe Int
getPos sq = case lookupCol sq of 
  Nothing -> Nothing
  Just col -> Just $ col + (read (tail sq) :: Int) * 9 - 9

updateSquare :: String -> Char -> String -> Maybe String
updateSquare sq val sudoku = case getPos sq of
  Nothing -> Nothing
  Just p -> if p > (length sudoku - 1) then Nothing 
            else Just $ replace p val sudoku

replace :: Int -> Char -> String -> String
replace i r s = [if j == i then r else c | (j, c) <- zip [0..] s]

interactiveSolve :: [String] -> IO ()
interactiveSolve (current:rest) = do
  putStrLn "Type 'solve' to solve this sudoku automatically, or enter a square and value: "
  input <- getLine
  case input of
    "quit"  -> return ()
    "solve" -> do
      printBoard $ solveSudoku current
      threadDelay 1000000
      loop rest
    _ -> do
      let assignment = (filter (/= "") . (splitString ' ')) input
          (sq, val) = (head assignment, (last . last) assignment)
          assignedSudoku = updateSquare sq val current
      case assignedSudoku >>= solveSudoku of
        Nothing -> do
          putStrLn "Invalid assignment. No change was made."
          printSudoku current
          threadDelay 1000000
          interactiveSolve (current:rest)
        solved -> do
          let s = fromJust assignedSudoku
          if (parseBoard s == solved) then do
            putStrLn "Solved!"
            loop rest
          else do
            putStrLn ("Valid assignment: " ++ sq ++ " " ++ [val])
            printSudoku s
            threadDelay 1000000
            interactiveSolve (s : rest)

loop :: [String] -> IO ()
loop [] = do
  putStrLn "Done!"
loop (current:rest) = do
  putStrLn $ "==============\nDetected " ++ (show . length $ (current:rest)) ++ " sudoku left in this file."
  putStrLn "Current sudoku:"
  printSudoku current
  putStrLn "1. Automatically solve the current sudoku."
  putStrLn "2. Solve all remaining sudokus in this file."
  putStrLn "3. Manually solve the current sudoku by assigning a value to a square."
  putStrLn "(Type 'quit' to exit.)"
  input <- getLine
  case input of
    "1" -> do
      printBoard $ solveSudoku current
      threadDelay 1000000
      loop rest
    "2" -> do
      let solvedSudokus = map solveSudoku (current:rest)
      mapM_ printBoard solvedSudokus
      putStrLn "Done!"
    "3" -> do
      interactiveSolve (current:rest)
    "quit" -> return ()
    _ -> do
      putStrLn "Invalid command. Try again."
      threadDelay 1000000
      loop (current:rest)

main :: IO ()
main = do
    putStrLn "Enter file name:"
    sudokuStrings <- getLine >>= readFile >>= parseFile
    loop sudokuStrings