module MapGenerator where

import AnsiUtils
import BoardUtils
import DataTypes
import Kodable (solvable)
import System.Random

-- Used to calculate neighbours
offsets = [(0, -1), (0, 1), (1, 0), (-1, 0)]

-- Checks if position lies inside board
isValid :: Position -> Int -> Int -> Bool
isValid (x, y) row col = (x >= 0) && (x < row) && (y >= 0) && (y < (col))

-- Generates a path from starting position to end position while making random decisions
genPath :: Position -> Position -> [Position] -> Int -> Int -> (Int, Int) -> IO [Position]
genPath (x, y) target path row col last
  | (x, y) == target = do
    return path
  | otherwise =
    do
      index <- randomRIO (0, 3)
      let (newXDif, newYDif) = offsets !! index
      prob <- randomRIO (0, 1)
      let (xDif, yDif) = [(newXDif, newYDif), last] !! prob
      let newX = x + xDif
      let newY = y + yDif
      let finalPath = path ++ [(newX, newY)]
      if isValid (newX, newY) row col
        then genPath (newX, newY) target finalPath row col (xDif, yDif)
        else genPath (x, y) target path row col last

-- Inserts Corresponding Element in Board
tileSymbol (x, y) path bonuses conds start target
  | (x, y) == start = Ball
  | (x, y) == target = Target
  | (x, y) `elem` bonuses = Star
  | (x, y) `elem` conds = Condition 'p'
  | (x, y) `elem` path = Path
  | otherwise = Grass

-- Generates a board of given dimensions
genMap :: Int -> Int -> IO Board
genMap row col = do
  startX <- randomRIO (0, row -1)
  endX <- randomRIO (0, row -1)
  path <- genPath (startX, 0) (endX, col - 1) [] row col (0, 1)
  bonus1 <- randomRIO (1, (length path) -2)
  bonus2 <- randomRIO (1, (length path) -2)
  bonus3 <- randomRIO (1, (length path) -2)
  cond1 <- randomRIO (1, (length path) -2)
  let condPos = [(x, y) | (x, y) <- path, y == col -1]
  cond2 <- randomRIO (0, (length condPos) -1)
  let bonuses = [path !! x | x <- [bonus1, bonus2, bonus3]]
  let conds = [path !! cond1, condPos !! cond2]
  let board = [[tileSymbol (x, y) path bonuses conds (startX, 0) (endX, col - 1) | y <- [0 .. (col -1)]] | x <- [0 .. (row -1)]]
  return board

-- Keeps generating boards till a solvable board is generated
genSolvableMaps :: Int -> Int -> IO Board
genSolvableMaps row col = do
  board <- genMap row col
  printBoard board
  let isSolvable = solvable board
  if (not isSolvable)
    then do
      putStrLn "Board not solvable! \n Generating new board..\n"
      genSolvableMaps row col
    else do
      putStrLn "Solvable board Generated!"
      return board

-- Generates a solvable board and saves it to a new file
genBoard :: IO ()
genBoard = do
  putStrLn "Enter Number of Rows: "
  row <- getLine
  putStrLn "Enter Number of Columns: "
  col <- getLine
  board <- genSolvableMaps (read row :: Int) (read col :: Int)
  fileNum <- randomRIO (10, 100)
  let fileName = "map" ++ show (fileNum :: Int) ++ "-2.txt"
  writeFile fileName (boardString board)
  putStrLn ("Map Saved to " ++ fileName)