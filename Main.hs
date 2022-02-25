module Main where

import           Data.List  (transpose)
import           Data.Maybe (isJust, isNothing)

type Board = [Row]
type Row   = [Maybe Player]

data Player = X | O
  deriving (Eq)

main :: IO ()
main = do
  let emptyBoard = replicate 3 (replicate 3 Nothing)
      count = length (allPlays X emptyBoard)
  putStr "All possible (valid) games: " >> print count
  putStr "Expected:                   " >> print 255168

allPlays :: Player -> Board -> [Board]
allPlays player board = do
  spot <- freeSpots board
  let board' = move player spot board
  if isFull board' || isWin board'
    then pure board'
    else allPlays (nextPlayer player) board'

isFull :: Board -> Bool
isFull = all (notElem Nothing)

isWin :: Board -> Bool
isWin board = any (any allSame) [diags board, board, transpose board]
  where
    allSame [a,b,c] = isJust a && a == b && b == c
    allSame _       = False
    diags [[a,_,a'], [_,b,_], [c',_,c]] = [[a,b,c], [a',b,c']]
    diags _                             = []

move :: Player -> (Int, Int) -> Board -> Board
move player (x, y) = update x $ update y (const $ Just player)

nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X

freeSpots :: Board -> [(Int, Int)]
freeSpots board = filter isFree ((,) <$> [0..2] <*> [0..2])
  where isFree (i, j) = isNothing (board !! i !! j)

update :: Int -> (a -> a) -> [a] -> [a]
update i f = zipWith update [0..]
  where update j x = if i == j then f x else x
