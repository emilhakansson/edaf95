-- Author: Emil Håkansson, Felicia Huynh

-- How to use:
-- 1. Open ghci
-- 2. Load the module: ':load SudokuSolver.hs'
-- 3. Run the main function: 'main'
-- 5. Type the name of the text file to solve.
-- 4. Follow the instructions on screen.

module SudokuSolver where

-- by Adrian Roth

import Data.Bool
import Data.Char
import Data.List
import Data.Maybe
import Data.List (intersperse)
--import Data.List.Split

fromMaybe :: a -> Maybe a -> a
fromMaybe defaultVal maybeVal =
  case maybeVal of
    Just val -> val
    Nothing -> defaultVal

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

fullBoard :: Board
fullBoard = [("A1", [1, 2]), ("A2", [2]), ("A3", [3]), ("A4", [4])]

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
  | otherwise = x: mapIf f pred xs

maybeOr :: Maybe a -> Maybe a -> Maybe a
maybeOr m1 m2
  | isJust m1 = m1
  | isJust m2 = m2
  | otherwise = Nothing

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

maybeBind :: Maybe a -> (a -> Maybe b) -> Maybe b
maybeBind x f
  | isNothing x = Nothing
  | otherwise   = f (fromJust x)

tryReplace :: Eq a => a -> a -> [a] -> Maybe [a]
tryReplace _ _ [] = Nothing
tryReplace y y' (x:xs)
  | x == y    = Just (y':xs)
  | otherwise = fmap (x:) $ tryReplace y y' xs

doIt = Just [1,2,3] >>= tryReplace 1 3 >>=
    tryReplace 3 2 >>= tryReplace 2 1

-- Takes the first element in xs, looks for the first occurence of it in zs, and tries to replace it with y.
-- Returns a Maybe [a], which can be passed to the >>= operator, along with a function of type ([a] -> Maybe [a]).
-- "recursiveReplacement xs ys" itself is a partially applied function, and has exactly the return type ([a] -> Maybe [a]),
-- so it can be passed to >>=, repeating the tryReplace with the next values of x and y.
-- Example: recursiveReplacement [1,2,3] [4,3,2] [2,3,1] first replaces 1 with 4: [2,3,4],
-- then replaces 2 with 3: [3,3,4], and finally 3 with 2: [2,3,4].
recursiveReplacement :: Eq a => [a] -> [a] -> [a] -> Maybe [a]
recursiveReplacement _ _ []  = Nothing -- Empty list? Return Nothing
recursiveReplacement _ [] zs = Just zs -- Nothing to replace? Just zs.
recursiveReplacement [] _ zs = Just zs
recursiveReplacement (x:xs) (y:ys) zs = tryReplace x y zs >>= recursiveReplacement xs ys

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
  | vals == []    = Nothing
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

-- Hint: use map, bind, assign, firstJust, lookupList
-- lookupList sq board = validNbrs
-- map -> assign -> 
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

printSudoku :: String -> IO ()
printSudoku str = do
  let sep = "==========="
      delim = "|"
      rowIndex = concat ((intersperse " ") rowBoxes)
      rows = splitEvery 9 str
      prettyRows = map (concat . (intersperse delim) . (splitEvery 3)) rows
  putStrLn sep
  putStrLn rowIndex
  mapM_ putStrLn prettyRows

printBoard :: Maybe Board -> IO ()
printBoard board = do
  let prettyBoard = concatMap show $ concatMap snd $ fromJust board
  printSudoku prettyBoard

colIndices :: [(String, Int)]
colIndices = [("A", 0), ("B", 1), ("C", 2), ("D", 3), ("E", 4), ("F", 5), ("G", 6), ("H", 7), ("I", 8)]

lookupCol :: String -> Int
lookupCol sq = fromJust (lookup ((head sq) : "") colIndices)
  
getPos :: String -> Int
getPos sq = (lookupCol sq) + (read (tail sq) :: Int) * 9 - 9
      
updateSquare :: Char -> String -> String -> String
updateSquare val sq sudoku = replace (getPos sq) val sudoku

replace :: Int -> Char -> String -> String
replace i r s = [if j == i then r else c | (j, c) <- zip [0..] s]

-- TODO: user input, solve sudoku, assign value.

interactiveSolve :: [String] -> Maybe Board -> IO ()
interactiveSolve sudokuStrings currentBoard = do
  let sudokuBoards = map solveSudoku sudokuStrings
  putStrLn "Type 'solve' to solve automatically, or enter square and value: "
  input <- getLine
  if (input == "solve") then do
    let solvedBoard = solveSudoku $ head sudokuStrings
        solvedNbrs = concatMap show $ concatMap snd $ fromJust solvedBoard
    printSudoku solvedNbrs
  else do
    let sqval = (filter (/= "") . (splitString ' ')) input
        sq = head sqval
        val = (last . last) sqval
        valInt = read [val] :: Int
        currentSudoku = head sudokuStrings
        assignedSudoku = updateSquare val sq currentSudoku
    case (solveSudoku assignedSudoku) of
      Nothing -> do 
        print "Invalid"
        interactiveSolve (currentSudoku : (tail sudokuStrings)) currentBoard
      Just x -> do 
        let assignedBoard = assign valInt sq $ fromJust . head $ sudokuBoards
        printSudoku (updateSquare val sq (head sudokuStrings))
        case assignedBoard of
          Nothing -> print "Invalid"
          Just x -> print "Valid"
        interactiveSolve (assignedSudoku : (tail sudokuStrings)) assignedBoard

loop :: [String] -> IO ()
loop [] = do
  putStrLn "Done!"
loop sudokuStrings = do
  putStrLn "==============\n"
  putStrLn "1. Solve the sudoku below:"
  printSudoku $ head sudokuStrings
  putStrLn "\n2. Solve all sudokus in this file.\n3. Assign a value to a square"
  input <- getLine
  if (input == "1") then do
    let solvedBoard = solveSudoku $ head sudokuStrings
    case solvedBoard of
      Nothing -> putStrLn "Invalid sudoku!"
      Just x -> do
        let solvedNbrs = concatMap show $ concatMap snd $ fromJust solvedBoard
        printSudoku solvedNbrs
    loop $ tail sudokuStrings
  else if (input == "2") then do
    let solvedSudokus = map solveSudoku sudokuStrings
    mapM_ (\b -> if isJust b then do
      let solvedSudoku = fromJust b
          solvedNbrs = concatMap snd solvedSudoku
          solvedString = concatMap show solvedNbrs
      printSudoku solvedString
      else putStrLn "Invalid sudoku!") solvedSudokus
    
  else if (input == "3") then do
    interactiveSolve sudokuStrings (parseBoard $ head sudokuStrings)  
  else return ()

main :: IO ()
main = do
    putStrLn "Enter file name:"
    sudokuStrings <- getLine >>= readFile >>= parseFile
    loop sudokuStrings

    --let boardList = map parseBoard sudokuStrings
    --mapM_ printSudoku sudokuStrings