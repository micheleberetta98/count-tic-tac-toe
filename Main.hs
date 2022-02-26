module Main where

import           Control.Monad (guard)
import           Data.List     (transpose)
import           Data.Maybe    (isJust, isNothing)

type Board = [Row]
type Row   = [Maybe Player]

data Player = X | O
  deriving (Show, Eq)

rows, cols :: Int
rows = 3
cols = 3

main :: IO ()
main = do
  let emptyBoard = replicate rows (replicate cols Nothing)
      count = length (allPlays X emptyBoard)
  putStr "All possible (valid) games: " >> print count
  putStr "Expected:                   " >> print 255168

allPlays :: Player -> Board -> [Board]
allPlays player board = do
  board' <- moves player board
  if isFull board' || isWin board'
    then pure board'
    else allPlays (next player) board'

isFull :: Board -> Bool
isFull = all (notElem Nothing)

isWin :: Board -> Bool
isWin board = any (any allSame) [diags board, board, transpose board]
  where
    allSame [a,b,c] = isJust a && a == b && b == c
    allSame _       = False
    diags [[a,_,a'], [_,b,_], [c',_,c]] = [[a,b,c], [a',b,c']]
    diags _                             = []

next :: Player -> Player
next X = O
next O = X

moves :: Player -> Board -> [Board]
moves p board = chunk . set p board' <$> freeSpots board'
  where
    board' = concat board
    set _ [] _     = []
    set v (_:xs) 0 = Just v : xs
    set v (x:xs) n = x : set v xs (n-1)

freeSpots :: [Maybe Player] -> [Int]
freeSpots = map fst . filter (isNothing . snd) . zip [0..]

chunk :: [a] -> [[a]]
chunk [] = []
chunk xs = y : chunk rest
  where (y, rest) = splitAt cols xs
