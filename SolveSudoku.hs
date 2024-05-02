module SolveSudoku where

-- by Adrian Roth

import Data.Bool
import Data.Char
import Data.List
import Data.Maybe

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

{-
parseSquare :: (String, Char) -> Board -> Maybe Board
parseSquare (s, x) values
  | x == '.' || x == '0' = return values
  | isDigit x = assign (digitToInt x) s values
  | otherwise = fail "not a valid grid"


parseBoard :: String -> Maybe Board
parseBoard = foldr ((=<<) . parseSquare) (Just emptyBoard) . zip squares
-}

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

eliminate :: Int -> String -> Board -> Maybe Board
eliminate val sq board
  | length vals == 1 && head vals == val = Nothing
  | lookupList sq board == [] = Nothing
  | otherwise = Just (eliminateValue val sq board) where
    vals = lookupList sq board

-- Assigns a square in the board a given value, and eliminates that value from its peers.
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