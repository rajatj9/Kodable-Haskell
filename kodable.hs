import Data.List (minimumBy)
import Data.Ord (comparing)
import System.Directory (doesFileExist)
import System.IO ()

data Tile = Grass | Ball | Condition | Func | Loop | Star | Path | Target deriving (Show, Eq)

data Action = UP | DOWN | RIGHT | LEFT | START | Cond Action | LOOP (Action, Action) Int deriving (Show, Eq)

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

enumerator :: [[Tile]] -> [(Int, Int, Tile)]
enumerator board = concat [[(x, y, val) | (y, val) <- enumerate row] | (x, row) <- enumerate board]

findBall :: [(Int, Int, Tile)] -> (Int, Int)
findBall ((x, y, val) : xs) = if val == Ball then (x, y) else findBall xs

findTarget :: [(Int, Int, Tile)] -> (Int, Int)
findTarget ((x, y, val) : xs) = if val == Target then (x, y) else findTarget xs

-- return only reachable bonuses
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
blockNodes = [Condition]

isBlockNode :: [[Tile]] -> (Int, Int) -> Bool
isBlockNode board (x, y) = ((board !! x) !! y) `elem` blockNodes

removeStar :: [[Tile]] -> (Int, Int) -> [[Tile]]
removeStar board (x, y) = [[if x1 == x && y == y1 && tile == Star then Path else tile | (y1, tile) <- enumerate row] | (x1, row) <- enumerate board]

manhattanDistance :: Num a => (a, a) -> (a, a) -> a
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

closestBonus :: (Ord a, Num a, Num a) => (a, a) -> [(a, a)] -> ((a, a), a)
closestBonus (x1, y1) bonuses = minimumBy (comparing snd) bonusAndDistance
  where
    newBonuses = [(x, y) | (x, y) <- bonuses, (x1, y1) /= (x, y)]
    bonusAndDistance = zip bonuses (map (manhattanDistance (x1, y1)) newBonuses)

heuristic :: [[Tile]] -> (Int, Int) -> Int
heuristic board (x, y) =
  if null bonuses
    then manhattanDistance (x, y) (findTarget enumeratedBoard)
    else bonusDist + heuristic (removeStar board bonusPos) bonusPos
  where
    enumeratedBoard = enumerator board
    bonuses = findBonuses enumeratedBoard
    (bonusPos, bonusDist) = closestBonus (x, y) bonuses

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

notMove :: [[Tile]] -> (Int, Int) -> (Int, Int) -> Bool
notMove board (x, y) (xNew, yNew) = ((board !! x !! y) `elem` blockNodes) || ((board !! xNew) !! yNew == Grass) || ((board !! x) !! y == Target && null (findBonuses (enumerator board)))

applyContinous :: [[Tile]] -> (Int, Int) -> Action -> Bool -> ((Int, Int), Action, [[Tile]])
applyContinous board (x, y) UP forced =
  if (x - 1 < 0) || (notMove board (x, y) (x -1, y) && not forced)
    then ((x, y), UP, board)
    else applyContinous (removeStar board (x, y)) (x - 1, y) UP False
applyContinous board (x, y) DOWN forced =
  if x + 1 >= length board || (notMove board (x, y) (x + 1, y) && not forced)
    then ((x, y), DOWN, board)
    else applyContinous (removeStar board (x, y)) (x + 1, y) DOWN False
applyContinous board (x, y) LEFT forced =
  if y - 1 < 0 || (notMove board (x, y) (x, y -1) && not forced)
    then ((x, y), LEFT, board)
    else applyContinous (removeStar board (x, y)) (x, y - 1) LEFT False
applyContinous board (x, y) RIGHT forced =
  if y + 1 >= length (head board) || (notMove board (x, y) (x, y + 1) && not forced)
    then ((x, y), RIGHT, board)
    else applyContinous (removeStar board (x, y)) (x, y + 1) RIGHT False

getThreeTupleHead :: (a, b, c) -> a
getThreeTupleHead (pos, _, _) = pos

getSolverSuccessors :: [[Tile]] -> (Int, Int) -> [((Int, Int), Action, [[Tile]])]
getSolverSuccessors board state = [applyContinous board state action (isBlockNode board state) | action <- actions, validAction board state action, getThreeTupleHead (applyContinous board state action (isBlockNode board state)) /= state]

getStateAndBonuses :: ((Int, Int), Action, [[Tile]]) -> ((Int, Int), Int)
getStateAndBonuses (pos, action, board) = (pos, length (findBonuses (enumerator board)))

getSolverSuccessorsBFS :: [[Tile]] -> (Int, Int) -> [((Int, Int), Int)] -> [((Int, Int), Action, [[Tile]])]
getSolverSuccessorsBFS board state visited = [applyContinous board state action (isBlockNode board state) | action <- actions, validAction board state action, getStateAndBonuses (applyContinous board state action (isBlockNode board state)) `notElem` visited]

exploreNeighbours :: ([((Int, Int), Action)], [((Int, Int), Int)], [[Tile]]) -> [([((Int, Int), Action)], [((Int, Int), Int)], [[Tile]])]
exploreNeighbours frontier
  | ((board !! x) !! y) == Target = [frontier]
  | null successors = []
  | otherwise =
    [ ( path ++ [(newState, newAction)],
        visited ++ [(newState, length (findBonuses (enumerator newBoard)))],
        newBoard
      )
      | (newState, newAction, newBoard) <- successors,
        (newState, length (findBonuses (enumerator newBoard))) `notElem` visited
    ]
  where
    (path, visited, board) = frontier
    (state, lastAction) = last path
    (x, y) = state
    successors = getSolverSuccessors board state

-- _______________ [([(Position, Action  )], [(Position), RemBo],    Board)] -> [([(Position, Action  )], [(Position), RemBo],    Board)]
findSolutionBFS :: [([((Int, Int), Action)], [((Int, Int), Int)], [[Tile]])] -> [([((Int, Int), Action)], [((Int, Int), Int)], [[Tile]])]
findSolutionBFS stack
  | stack == newStack = stack -- All frontiers have reached Target (no new neighbours)
  | otherwise = findSolutionBFS newStack
  where
    newStack = concat [exploreNeighbours frontier | frontier <- stack]

solveBFS :: [[Tile]] -> [([((Int, Int), Action)], [((Int, Int), Int)], [[Tile]])]
solveBFS board = findSolutionBFS [([(ballPos, START)], [(ballPos, initBonus)], board)]
  where
    ballPos = findBall (enumerator board)
    initBonus = length (findBonuses $ enumerator board)

findUnCollectableBonuses :: [([((Int, Int), Action)], [((Int, Int), Int)], [[Tile]])] -> Int
findUnCollectableBonuses stack = minimum [snd (last visited) | (path, visited, board) <- stack]

findCompletePaths :: [([((Int, Int), Action)], [((Int, Int), Int)], [[Tile]])] -> [[((Int, Int), Action)]]
findCompletePaths stack = [path | (path, visited, _) <- stack, snd (last visited) == unCollectableBonuses]
  where
    unCollectableBonuses = findUnCollectableBonuses stack

parsePathConditions :: [((Int, Int), Action)] -> Bool -> [[Tile]] -> [((Int, Int), Action)]
parsePathConditions [] _ _ = []
parsePathConditions (((x, y), action) : remPath) prevCondition board = if prevCondition then ((x, y), Cond action) : parsedPath else ((x, y), action) : parsedPath
  where
    nextCond = (board !! x) !! y `elem` blockNodes
    parsedPath = parsePathConditions remPath nextCond board

loopAccumulator :: (Action, Action) -> [Action] -> Int -> (Int, [Action])
loopAccumulator (action1, action2) stack count
  | length stack < 2 = (count, stack)
  | otherwise = if stack !! 0 == action1 && stack !! 1 == action2 then loopAccumulator (action1, action2) (drop 2 stack) (count + 1) else (count, stack)

createLoops :: [Action] -> [Action]
createLoops stack
  | length stack < 4 = stack
  | otherwise = if action1 == (stack !! 2) && action2 == (stack !! 3) then result else head stack : createLoops (tail stack)
  where
    action1 = head stack
    action2 = stack !! 1
    (loopCount, remList) = loopAccumulator (action1, action2) (drop 4 stack) 2
    result = LOOP (action1, action2) loopCount : createLoops remList

mainBFS :: IO ()
mainBFS = do
  putStrLn "Enter File Name: "
  fileName <- getLine
  board <- load fileName
  putStrLn "Board loaded successfully!"
  solvable <- check board
  putStrLn ("Solvable: " ++ show solvable)
  let completePaths = findCompletePaths (solveBFS board)
  putStrLn $ concat ["Path: " ++ show path ++ "\n" | path <- completePaths]
  putStrLn ("Number of Paths: " ++ show (length completePaths))
  let optimalPath = minimumBy (comparing length) completePaths
  let parsedOptimalPath = parsePathConditions optimalPath False board
  let optimalPathActions = [action | (_, action) <- parsedOptimalPath]
  let optimalPathWithLoops = createLoops optimalPathActions
  putStrLn ("Optimal Path: " ++ show optimalPathWithLoops)

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
