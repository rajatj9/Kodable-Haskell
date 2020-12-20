import BoardUtils
import DataTypes
import System.Random

-- type Position = (Int, Int)

offsets = [(0, -1), (0, 1), (1, 0), (-1, 0)]

isValid :: Position -> Int -> Int -> Bool
isValid (x, y) row col = (x >= 0) && (x < row) && (y >= 0) && (y < col)

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
      if isValid (newX, newY) row col
        then genPath (newX, newY) target (path ++ [(newX, newY)]) row col (x, y)
        else genPath (x, y) target path row col last

tileSymbol (x, y) path bonuses start target
  | (x, y) == start = Ball
  | (x, y) == target = Target
  | (x, y) `elem` bonuses = Star
  | (x, y) `elem` path = Path
  | otherwise = Grass

genMap :: Int -> Int -> IO ()
genMap row col = do
  startX <- randomRIO (0, row -1)
  endX <- randomRIO (0, row -1)
  path <- genPath (startX, 0) (endX, col - 1) [] row col (0, 1)
  let pathTiles = tail $ init $ path
  bonus1 <- randomRIO (1, (length path) -2)
  bonus2 <- randomRIO (1, (length path) -2)
  bonus3 <- randomRIO (1, (length path) -2)
  let bonuses = [path !! x | x <- [bonus1, bonus2, bonus3]]
  let board = [[tileSymbol (x, y) path bonuses (startX, 0) (endX, col - 1) | y <- [0 .. (col -1)]] | x <- [0 .. (row -1)]]
  putStrLn (boardString board)
  fileNum <- randomRIO (10, 100)
  let fileName = "map" ++ show (fileNum :: Int) ++ "-2.txt"
  writeFile fileName (boardString board)
  putStrLn ("Map Saved to " ++ fileName)

main = genMap 10 20