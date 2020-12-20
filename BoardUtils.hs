module BoardUtils where

import DataTypes
import System.Directory (doesFileExist)

allSameLength :: [[String]] -> Bool
allSameLength [] = True
allSameLength xs = all null xs || all (not . null) xs && allSameLength (map tail xs)

validChars :: [Char]
validChars = ['*', '-', 'p', 'o', 'y', 'b', '@', 't', ' ']

validRow :: [Char] -> Bool
validRow = foldr (\x -> (&&) (x `elem` validChars)) True

getFile :: String -> IO [String]
getFile fileName = do
  fileChecker <- doesFileExist fileName
  if fileChecker
    then do
      fileContents <- readFile fileName
      let fileLines = lines fileContents
      if null fileLines
        then do
          putStrLn "Empty File! Try again."
          return []
        else
          if not (allSameLength (map words fileLines))
            then do
              putStrLn "Invalid Board, Lines have different length. Try again."
              return []
            else
              if all validRow fileLines
                then return fileLines
                else do
                  putStrLn "Invalid Board, File contains invalid characters. Try Again."
                  return []
    else do
      putStrLn "Invalid input! The file does not exist in this directory. Try Again."
      return []

mapCharToData :: Char -> Tile
mapCharToData '*' = Grass
mapCharToData '@' = Ball
mapCharToData '-' = Path
mapCharToData 't' = Target
mapCharToData 'b' = Star
mapCharToData 'p' = Condition 'p'
mapCharToData 'o' = Condition 'o'
mapCharToData 'y' = Condition 'y'

boardString :: Board -> String
boardString [x] = unwords (map show x)
boardString (x : xs) = unwords (map show x) ++ "\n" ++ boardString xs

boardWithBall :: Board -> Position -> Board
boardWithBall board (x, y) = [[if x1 == x && y1 == y then Ball else newTile tile | (y1, tile) <- enumerate row] | (x1, row) <- enumerate board]
  where
    newTile tile = if tile == Ball then Path else tile

parseLine :: [Char] -> [Tile]
parseLine inputs = [mapCharToData x | x <- inputs, x /= ' ']

makeBoard :: [[Char]] -> Board
makeBoard = foldr ((:) . parseLine) []

enumerate :: [b] -> [(Int, b)]
enumerate = zip [0 ..]

enumerator :: Board -> [(Int, Int, Tile)]
enumerator board = concat [[(x, y, val) | (y, val) <- enumerate row] | (x, row) <- enumerate board]

findBall :: [(Int, Int, Tile)] -> Position
findBall ((x, y, val) : xs) = if val == Ball then (x, y) else findBall xs

findTarget :: [(Int, Int, Tile)] -> Position
findTarget ((x, y, val) : xs) = if val == Target then (x, y) else findTarget xs

-- return only reachable bonuses
findBonuses :: [(Int, Int, Tile)] -> [Position]
findBonuses [] = []
findBonuses ((x, y, val) : xs) = if val == Star then (x, y) : findBonuses xs else findBonuses xs
