module Main where

import           Data.List   (transpose)
import           Data.Maybe  (isJust, isNothing)
import           Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V

type Board = Vector Row
type Row   = Vector (Maybe Player)

data Player = X | O
  deriving (Eq)

main :: IO ()
main = do
  let emptyBoard = V.replicate 3 (V.replicate 3 Nothing)
      count = length (allPlays X emptyBoard)
  putStr "All possible (valid) games: " >> print count
  putStr "Expected:                   " >> print 255168

allPlays :: Player -> Board -> [Board]
allPlays player board = do
  spot <- freeSpots board
  let board' = play board player spot
  if isFull board' || isWin board'
    then pure board'
    else allPlays (nextPlayer player) board'

isFull :: Board -> Bool
isFull = all (notElem Nothing)

isWin :: Board -> Bool
isWin board = any (any allSame) [board', transpose board', diags board']
  where
    board' = map V.toList $ V.toList board
    allSame [a,b,c] = isJust a && a == b && b == c
    allSame _       = False
    diags [[a,_,a'], [_,b,_], [c',_,c]] = [[a,b,c], [a',b,c']]
    diags _                             = []

play :: Board -> Player -> (Int, Int) -> Board
play board player (x, y) = board // [(x, row')]
  where
    row' = (board ! x) // [(y, Just player)]

nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X

freeSpots :: Board -> [(Int, Int)]
freeSpots board = filter isFree ((,) <$> [0..2] <*> [0..2])
  where isFree (i, j) = isNothing (board ! i ! j)

updateList :: [a] -> a -> Int -> [a]
updateList lst value i = zipWith update [0..] lst
  where update j x = if i == j then value else x
