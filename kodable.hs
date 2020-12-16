import Data.List (minimumBy)
import Data.Ord (comparing)
import System.Directory (doesFileExist)
import System.IO ()

data Tile = Grass | Ball | Condition | Func | Loop | Star | Path | Target deriving (Show, Eq)

data Action = UP | DOWN | RIGHT | LEFT deriving (Show, Eq)

actions :: [Action]
actions = [LEFT, UP, DOWN, RIGHT]

getFile :: String -> IO [String]
getFile fileName = do
  fileChecker <- doesFileExist fileName
  if fileChecker
    then do
      fileContents <- readFile fileName
      let fileLines = lines fileContents
      return fileLines
    else do
      putStrLn "Invalid input! The file does not exist in this directory"
      return []

load :: String -> IO [[Tile]]
load fileName = do
  fileContent <- getFile fileName
  return (makeBoard fileContent)

mapCharToData :: Char -> Tile
mapCharToData '*' = Grass
mapCharToData '@' = Ball
mapCharToData '-' = Path
mapCharToData 't' = Target
mapCharToData 'b' = Star
mapCharToData 'p' = Condition
mapCharToData 'o' = Func
mapCharToData 'y' = Loop

parseLine :: [Char] -> [Tile]
parseLine inputs = [mapCharToData x | x <- inputs, x /= ' ']

makeBoard :: [[Char]] -> [[Tile]]
makeBoard = foldr ((:) . parseLine) []

enumerate :: [b] -> [(Int, b)]
enumerate = zip [0 ..]

enumerator board = concat [[(x, y, val) | (y, val) <- enumerate row] | (x, row) <- enumerate board]

findBall :: [(Int, Int, Tile)] -> (Int, Int)
findBall ((x, y, val) : xs) = if val == Ball then (x, y) else findBall xs

findTarget :: [(Int, Int, Tile)] -> (Int, Int)
findTarget ((x, y, val) : xs) = if val == Target then (x, y) else findTarget xs

findBonuses :: [(Int, Int, Tile)] -> [(Int, Int)]
findBonuses [] = []
findBonuses ((x, y, val) : xs) = if val == Star then (x, y) : findBonuses xs else findBonuses xs

applyAction :: [[Tile]] -> (Int, Int) -> Action -> (Int, Int)
applyAction board (x, y) UP = if ((board !! (x - 1)) !! y) /= Grass then (x - 1, y) else (-1, -1)
applyAction board (x, y) DOWN = if ((board !! (x + 1)) !! y) /= Grass then (x + 1, y) else (-1, -1)
applyAction board (x, y) LEFT = if ((board !! x) !! (y - 1)) /= Grass then (x, y - 1) else (-1, -1)
applyAction board (x, y) RIGHT = if ((board !! x) !! (y + 1)) /= Grass then (x, y + 1) else (-1, -1)

validAction :: [[Tile]] -> (Int, Int) -> Action -> Bool
validAction _ (x, _) UP = (x - 1) >= 0
validAction _ (_, y) LEFT = (y - 1) >= 0
validAction board (_, y) RIGHT = (y + 1) < length (head board)
validAction board (x, _) DOWN = (x + 1) < length board

getSuccessors :: [[Tile]] -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
getSuccessors board state visited = [applyAction board state action | action <- actions, validAction board state action, applyAction board state action /= (-1, -1) && not (elem (applyAction board state action) visited)]

dfs :: [[Tile]] -> [[(Int, Int)]] -> [(Int, Int)] -> IO Bool
dfs board stack visited
  | null stack = do
    return False
  | ((board !! x) !! y) == Target = do
    return True
  | any (\(x, y) -> ((board !! x) !! y) == Target) (getSuccessors board (x, y) newVisited) = do
    return True
  | otherwise = do dfs board (init stack ++ newNodes) newVisited
  where
    node = last stack
    (x, y) = last (last stack)
    newVisited = visited ++ [(x, y)]
    neighbours = getSuccessors board (x, y) newVisited
    newNodes =
      if not (null neighbours) then [node ++ [neighbour] | neighbour <- neighbours] else []

check :: [[Tile]] -> IO Bool
check board = dfs board stack visited
  where
    stack = [[findBall (enumerator board)]]
    visited = []

-- Solver

blockNodes :: [Tile]
blockNodes = [Grass]

removeStar :: [[Tile]] -> (Int, Int) -> [[Tile]]
removeStar board (x, y) = [[if x1 == x && y == y1 && tile == Star then Path else tile | (y1, tile) <- enumerate row] | (x1, row) <- enumerate board]

-- Visited will store (X_pos, Y_pos, Action, Num_Stars_in_the_Game_then)
-- So the algorithm doesn't take the same action when the Num_Stars_in_the_Game doesn't change
-- Make ball move based on distance to the closest star
-- Make the ball move towards the target if all the stars have been collected

manhattanDistance :: Num a => (a, a) -> (a, a) -> a
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

closestBonus :: (Ord a, Num a, Num a) => (a, a) -> [(a, a)] -> ((a, a), a)
-- closestBonus (x1, y1) bonuses = foldl (\(pos1, d1) (pos2, d2) -> if d1 < d2 then (pos1, d1) else (pos2, d2)) ((x1, y1), 100000000) bonusAndDistance
--   where
--     newBonuses = [(x, y) | (x, y) <- bonuses, (x1, y1) /= (x, y)]
--     bonusAndDistance = zip bonuses (map (manhattanDistance (x1, y1)) bonuses)
closestBonus (x1, y1) bonuses = minimumBy (comparing snd) bonusAndDistance
  where
    newBonuses = [(x, y) | (x, y) <- bonuses, (x1, y1) /= (x, y)]
    bonusAndDistance = zip bonuses (map (manhattanDistance (x1, y1)) bonuses)

heuristic :: [[Tile]] -> (Int, Int) -> Int
heuristic board (x, y) =
  if null bonuses
    then manhattanDistance (x, y) (findTarget enumeratedBoard)
    else bonusDist + heuristic (removeStar board bonusPos) bonusPos
  where
    enumeratedBoard = enumerator board
    bonuses = findBonuses enumeratedBoard
    (bonusPos, bonusDist) = closestBonus (x, y) bonuses

heuristic2 :: [[Tile]] -> (Int, Int) -> Int
heuristic2 board (x, y) = manhattanDistance (x, y) (findTarget enumeratedBoard) + sum (map (manhattanDistance (x, y)) bonuses)
  where
    bonuses = findBonuses enumeratedBoard
    enumeratedBoard = enumerator board

notMove board (x, y) (xNew, yNew) = ((board !! x !! y) `elem` blockNodes) || ((board !! xNew) !! yNew == Grass) || ((board !! x) !! y == Target && null (findBonuses (enumerator board)))

-- Need to remove the stars which were accumulated and also take into account borders
applyContinous :: [[Tile]] -> (Int, Int) -> Action -> ((Int, Int), Action, [[Tile]])
applyContinous board (x, y) UP =
  if (x - 1 < 0) || ((board !! (x - 1) !! y) `elem` blockNodes) || ((board !! x) !! y == Target && null (findBonuses (enumerator board)))
    then ((x, y), UP, board)
    else applyContinous (removeStar board (x, y)) (x - 1, y) UP
applyContinous board (x, y) DOWN =
  if x + 1 >= length (head board) || (board !! (x + 1)) !! y `elem` blockNodes || ((board !! x) !! y == Target && null (findBonuses (enumerator board)))
    then ((x, y), DOWN, board)
    else applyContinous (removeStar board (x, y)) (x + 1, y) DOWN
applyContinous board (x, y) LEFT =
  if y - 1 < 0 || ((board !! x) !! (y -1)) `elem` blockNodes || ((board !! x) !! y == Target && null (findBonuses (enumerator board)))
    then ((x, y), LEFT, board)
    else applyContinous (removeStar board (x, y)) (x, y - 1) LEFT
applyContinous board (x, y) RIGHT =
  if y + 1 >= length (head board) || ((board !! x) !! (y + 1)) `elem` blockNodes || ((board !! x) !! y == Target && null (findBonuses (enumerator board)))
    then ((x, y), RIGHT, board)
    else applyContinous (removeStar board (x, y)) (x, y + 1) RIGHT

-- new getSuccessors such that it keeps applying the function till it reaches

getThreeTupleHead :: (a, b, c) -> a
getThreeTupleHead (pos, _, _) = pos

getSolverSuccessors :: [[Tile]] -> (Int, Int) -> [((Int, Int), Action, [[Tile]])]
getSolverSuccessors board state = [applyContinous board state action | action <- actions, validAction board state action, getThreeTupleHead (applyContinous board state action) /= state]

findSolution :: [[Tile]] -> (Int, Int) -> [Action] -> IO ()
findSolution board state solution
  | heuristic board state == 0 = print stateWithScores
  | otherwise = do
    putStrLn ("Current State: " ++ show state)
    putStrLn ("Possible States: " ++ show statesWithoutBoard)
    putStrLn ("Remaining Bonuses: " ++ show (findBonuses (enumerator board)))
    putStrLn ("Heuristic at Current State: " ++ show (heuristic board state))
    putStrLn ""
    findSolution newBoard newState (solution ++ [action])
  where
    possibleStates = getSolverSuccessors board state
    stateWithScores = map (\(newPos, action, possibleBoard) -> (heuristic possibleBoard newPos, newPos, action, possibleBoard)) possibleStates
    statesWithoutBoard = map (\(a, b, c, d) -> (a, b, c)) stateWithScores
    (f, newState, action, newBoard) = minimumBy (comparing (\(h, _, _, _) -> h)) stateWithScores

-- (f, newState, action, newBoard) = foldl (\(h1, pos1, a1, b1) (h2, pos2, a2, b2) -> if h1 < h2 then (h1, pos1, a1, b1) else (h2, pos2, a2, b2)) (1000000000, state, UP, board) stateWithScores

main :: IO ()
main = do
  putStrLn "Enter File Name: "
  fileName <- getLine
  board <- load fileName
  putStrLn "Board loaded successfully!"
  solvable <- check board
  putStrLn ("Solvable: " ++ show solvable)
  findSolution board (findBall (enumerator board)) []

sample = ["* * * * * - - - - - - - - - - - - - - - - - - - * * * * * ", "* * * * * b - - - - - - - - - - - - - - - - - b * * * * * ", "* * * * * - * * * * * * * * * * * * * * * * * - * * * * * ", "* * * * * - * * - - - - * * * * * - - - - * * - * * * * * ", "* * * * * - * * - y y - * * * * * - y y - * * - * * * * * ", "* * * * * - * * - - - - * * * * * - - - - * * - * * * * * ", "* * * * * - * * * * * * - - b - - * * * * * * - * * * * *  ", "* * * * * - * * * * * * - * * * - * * * * * * - * * * * * ", "@ - - - - - * * * * * * - * * * - * * * * * * p - - - - t ", "* * * * * - * * * * * * - * * * - * * * * * * - * * * * * ", "* * * * * - - - - - - - - * * * - - - - - - - - * * * * * ", "* * * * * * * * * * * * * * * * * * * * * * * * * * * * * ", "* * * * * * * * * * * * * * * * * * * * * * * * * * * * * "]

-- s1 = ["* t - ", "* * - ", "@ - - "]
s1 =
  [ [Grass, Target, Path],
    [Path, Path, Path],
    [Star, Grass, Path],
    [Ball, Path, Path]
  ]

s2 =
  [ [Grass, Target, Path],
    [Grass, Path, Path],
    [Star, Grass, Path],
    [Ball, Path, Path]
  ]

stack = [[findBall (enumerator (makeBoard sample))]]

func = minimumBy (comparing fst) [(1, 24), (3, 5), (5, 7)]