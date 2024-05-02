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
mapIf f1 pred (x:xs)
  | pred x         = f1 x : mapIf f1 pred xs
  | otherwise    = x: mapIf f1 pred xs

maybeOr :: Maybe a -> Maybe a -> Maybe a
maybeOr m1 m2
  | isJust m1 = m1
  | isJust m2 = m2
  | otherwise = Nothing

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (x:xs)
  | isJust x        = x
  | otherwise       = firstJust xs

lookupList :: Eq a => a -> [(a, [b])] -> [b]
lookupList _ [] = []
lookupList y ((a, bList): rest)
  | y == a    = bList
  | otherwise = lookupList y rest

maybeBind :: Maybe a -> (a -> Maybe b) -> Maybe b
maybeBind x f
  | isNothing x = Nothing
  | otherwise = f (fromJust x)

tryReplace :: Eq a => a -> a -> [a] -> Maybe [a]
tryReplace _ _ [] = Nothing
tryReplace y y' (x:xs)
  | x == y = Just (y':xs)
  | otherwise = fmap (x:) $ tryReplace y y' xs

doIt = Just [1,2,3] `maybeBind` tryReplace 1 3 `maybeBind`
    tryReplace 3 2 `maybeBind` tryReplace 2 1

recursiveReplacement :: Eq a => [a] -> [a] -> [a] -> Maybe [a]
recursiveReplacement _ _ [] = Nothing -- Empty list? Return Nothing
recursiveReplacement _ [] zs = Just zs -- Nothing to replace? Just zs
recursiveReplacement [] _ zs = Just zs
recursiveReplacement (x:xs) (y:ys) (z:zs)
  | z == x = (recursiveReplacement xs ys (z:zs)) `maybeBind` tryReplace x y
  | otherwise = fmap (z:) (recursiveReplacement (x:xs) (y:ys) zs)

setValue :: Int -> String -> Board -> Board
setValue val sq board = mapIf (\(square, vals) -> (square, [val])) (\(square, vals) -> square == sq) board

eliminateValue :: Int -> String -> Board -> Board
eliminateValue val sq board = mapIf (\(square, vals) -> (square, filter (/= val) vals)) (\(square, vals) -> square == sq) board

eliminate :: Int -> String -> Board -> Maybe Board
eliminate val sq board
  | length (lookupList sq board) == 1 && (head . fromJust) (lookup sq board) == val = Nothing
  | lookupList sq board == [] = Nothing
  | otherwise = Just (eliminateValue val sq board)

assign :: Int -> String -> Board -> Maybe Board
assign val sq board = assign' val sq (setValue val sq board)

assign' :: Int -> String -> Board -> Maybe Board
assign' val sq (b:board)
  | (fst b) `elem` peerList = eliminate val (fst b) board `maybeBind` assign' val sq
  | otherwise = fmap (b:) (assign' val sq board) where
    peerList = lookupList sq peers