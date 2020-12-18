import Data.Char
import Data.Function (on)
import Data.List (maximumBy, minimumBy, nub)
import Data.Ord (comparing)
import System.Directory (doesFileExist)
import System.IO ()
import Prelude hiding (Left, Right)

data Tile = Grass | Ball | Condition Char | Star | Path | Target deriving (Eq)

data Action = Up | Down | Right | Left | START | Cond Char Action | LOOP (Action, Action) Int | Function (Action, Action, Action) deriving (Show, Eq)

instance Show Tile where
  show Grass = "*"
  show Path = "-"
  show Star = "b"
  show Ball = "@"
  show (Condition 'p') = "p"
  show (Condition 'o') = "o"
  show (Condition 'y') = "y"
  show Target = "t"

actions :: [Action]
actions = [Left, Up, Down, Right]

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
mapCharToData 'p' = Condition 'p'
mapCharToData 'o' = Condition 'o'
mapCharToData 'y' = Condition 'y'

boardString :: [[Tile]] -> String
boardString [x] = unwords (map show x)
boardString (x : xs) = unwords (map show x) ++ "\n" ++ boardString xs

boardWithBall :: [[Tile]] -> (Int, Int) -> [[Tile]]
boardWithBall board (x, y) = [[if x1 == x && y1 == y then Ball else newTile tile | (y1, tile) <- enumerate row] | (x1, row) <- enumerate board]
  where
    newTile tile = if tile == Ball then Path else tile

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
applyAction board (x, y) Up = if ((board !! (x - 1)) !! y) /= Grass then (x - 1, y) else (-1, -1)
applyAction board (x, y) Down = if ((board !! (x + 1)) !! y) /= Grass then (x + 1, y) else (-1, -1)
applyAction board (x, y) Left = if ((board !! x) !! (y - 1)) /= Grass then (x, y - 1) else (-1, -1)
applyAction board (x, y) Right = if ((board !! x) !! (y + 1)) /= Grass then (x, y + 1) else (-1, -1)

validAction :: [[Tile]] -> (Int, Int) -> Action -> Bool
validAction _ (x, _) Up = (x - 1) >= 0
validAction _ (_, y) Left = (y - 1) >= 0
validAction board (_, y) Right = (y + 1) < length (head board)
validAction board (x, _) Down = (x + 1) < length board

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
blockNodes = [Condition 'p', Condition 'y', Condition 'o']

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
applyContinous board (x, y) Up forced =
  if (x - 1 < 0) || (notMove board (x, y) (x -1, y) && not forced)
    then ((x, y), Up, board)
    else applyContinous (removeStar board (x, y)) (x - 1, y) Up False
applyContinous board (x, y) Down forced =
  if x + 1 >= length board || (notMove board (x, y) (x + 1, y) && not forced)
    then ((x, y), Down, board)
    else applyContinous (removeStar board (x, y)) (x + 1, y) Down False
applyContinous board (x, y) Left forced =
  if y - 1 < 0 || (notMove board (x, y) (x, y -1) && not forced)
    then ((x, y), Left, board)
    else applyContinous (removeStar board (x, y)) (x, y - 1) Left False
applyContinous board (x, y) Right forced =
  if y + 1 >= length (head board) || (notMove board (x, y) (x, y + 1) && not forced)
    then ((x, y), Right, board)
    else applyContinous (removeStar board (x, y)) (x, y + 1) Right False

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
      | (newState, newAction, newBoard) <- successors
    ]
  where
    (path, visited, board) = frontier
    (state, lastAction) = last path
    (x, y) = state
    successors = getSolverSuccessorsBFS board state visited

findPathsBFS :: [([((Int, Int), Action)], [((Int, Int), Int)], [[Tile]])] -> [([((Int, Int), Action)], [((Int, Int), Int)], [[Tile]])]
findPathsBFS stack
  | stack == newStack = stack -- All frontiers have reached Target (no new neighbours)
  | otherwise = findPathsBFS newStack
  where
    newStack = concat [exploreNeighbours frontier | frontier <- stack]

findCompletePaths :: [[Tile]] -> [[((Int, Int), Action)]]
findCompletePaths board = extractCompletePaths $ findPathsBFS [([(ballPos, START)], [(ballPos, initBonus)], board)]
  where
    ballPos = findBall (enumerator board)
    initBonus = length (findBonuses $ enumerator board)

findUnCollectableBonuses :: [([((Int, Int), Action)], [((Int, Int), Int)], [[Tile]])] -> Int
findUnCollectableBonuses stack = minimum [snd (last visited) | (path, visited, board) <- stack]

extractCompletePaths :: [([((Int, Int), Action)], [((Int, Int), Int)], [[Tile]])] -> [[((Int, Int), Action)]]
extractCompletePaths stack = [path | (path, visited, _) <- stack, snd (last visited) == unCollectableBonuses]
  where
    unCollectableBonuses = findUnCollectableBonuses stack

findOptimalPath :: [[((Int, Int), Action)]] -> [[Tile]] -> [Action]
findOptimalPath path board = parsePath (minimumBy (comparing length) path) board

parsePathConditions :: [((Int, Int), Action)] -> Bool -> Char -> Action -> [[Tile]] -> [((Int, Int), Action)]
parsePathConditions [] _ _ _ _ = []
parsePathConditions (((x, y), action) : remPath) prevCondition color prevAction board
  | addCondition = ((x, y), Cond color action) : parsedPath
  | not prevCondition = ((x, y), action) : parsedPath
  | otherwise = parsedPath
  where
    addCondition = prevCondition && (action /= prevAction)
    nextCond = (board !! x) !! y `elem` blockNodes
    (Condition nextColor) = if nextCond then (board !! x) !! y else Condition 'n'
    parsedPath = parsePathConditions remPath nextCond nextColor action board

loopAccumulator :: (Action, Action) -> [Action] -> Int -> (Int, [Action])
loopAccumulator (action1, action2) stack count
  | length stack < 2 || count > 4 = (count, stack)
  | otherwise = if head stack == action1 && stack !! 1 == action2 then loopAccumulator (action1, action2) (drop 2 stack) (count + 1) else (count, stack)

createLoops :: [Action] -> [Action]
createLoops stack
  | length stack < 4 = stack
  | otherwise = if action1 == (stack !! 2) && action2 == (stack !! 3) then result else head stack : createLoops (tail stack)
  where
    action1 = head stack
    action2 = stack !! 1
    (loopCount, remList) = loopAccumulator (action1, action2) (drop 4 stack) 2
    result = LOOP (action1, action2) loopCount : createLoops remList

-- Change it to create a function with the most frequently occuring pattern.
createFunctions :: [Action] -> [Action]
createFunctions [a, b] = [a, b]
createFunctions [a] = [a]
createFunctions [] = []
createFunctions (LOOP (a1, a2) x1 : b : c : remList) = LOOP (a1, a2) x1 : createFunctions (b : c : remList)
createFunctions (b : LOOP (a1, a2) x1 : c : remList) = [b, LOOP (a1, a2) x1] ++ createFunctions (c : remList)
createFunctions (b : c : LOOP (a1, a2) x1 : remList) = [b, c, LOOP (a1, a2) x1] ++ createFunctions remList
createFunctions (a : b : c : remList) = Function (a, b, c) : createFunctions (b : c : remList)

extractFunctions [] = []
extractFunctions (Function (a, b, c) : remList) = Function (a, b, c) : extractFunctions remList
extractFunctions (x : remList) = extractFunctions remList

mostFreqFunction :: [Action] -> Action
mostFreqFunction list = fst $ maximumBy (compare `on` snd) elemCounts
  where
    elemCounts = nub [(element, count) | element <- list, let count = length (filter (== element) list)]

replaceWithFunction :: [Action] -> Action -> [Action]
replaceWithFunction actions fn
  | length actions < 3 = actions
  | otherwise = if head actions == a1 && actions !! 1 == a2 && actions !! 2 == a3 then fn : replaceWithFunction (drop 3 actions) fn else head actions : replaceWithFunction (tail actions) fn
  where
    Function (a1, a2, a3) = fn

addFunctionToPath :: [Action] -> [Action]
addFunctionToPath actions = if null functions then actions else replaceWithFunction actions (mostFreqFunction functions)
  where
    functions = extractFunctions $ createFunctions actions

parsePath :: [((Int, Int), Action)] -> [[Tile]] -> [Action]
parsePath optimalPath board = finalPathWithFunctions
  where
    parsedWithCond = parsePathConditions optimalPath False 'n' START board
    pathWithOnlyActions = [action | (_, action) <- parsedWithCond]
    parsedWithLoops = createLoops (tail pathWithOnlyActions) -- remove START
    finalPathWithFunctions = addFunctionToPath parsedWithLoops

solve :: [[Tile]] -> [Action]
solve board = findOptimalPath completePaths board
  where
    completePaths = findCompletePaths board

createActionList :: [String] -> IO [String]
createActionList actions = do
  if null actions then putStr "First Direction: " else putStr "Next Direction: "
  direction <- getLine
  if direction == "" then return actions else do createActionList (actions ++ [direction])

seperateByComma :: [Char] -> [[Char]]
seperateByComma str = words [if c == ',' then ' ' else c | c <- str]

parseAction :: [Char] -> Action
parseAction "Left" = Left
parseAction "Right" = Right
parseAction "Up" = Up
parseAction "Down" = Down
parseAction action
  | take 4 action == "Loop" = LOOP (parseAction action1Str, parseAction action2Str) loopFreq
  | take 4 action == "Cond" = Cond condColor condAction
  where
    insideBracket = init (drop 5 action)
    loopFreq = digitToInt (head insideBracket)
    insideActions = drop 3 insideBracket
    [action1Str, action2Str] = seperateByComma insideActions
    condAction = parseAction insideActions
    condColor = head insideBracket

applyLoop board state loopFreq (action1, action2)
  | loopFreq <= 0 = (state, board)
  | otherwise = applyLoop finalBoard finalState (loopFreq - 1) (action1, action2)
  where
    (newState, newBoard) = applyPlayAction action1 state board
    (finalState, finalBoard) = applyPlayAction action2 newState newBoard

extractPosAndBoard :: (a, b1, b2) -> (a, b2)
extractPosAndBoard = \(pos, _, board) -> (pos, board)

applyPlayAction :: Action -> (Int, Int) -> [[Tile]] -> ((Int, Int), [[Tile]])
applyPlayAction (LOOP (a1, a2) loopFreq) state board = applyLoop board state loopFreq (a1, a2)
applyPlayAction (Cond color action) state board = extractPosAndBoard $ applyContinous board state action True
applyPlayAction action state board = extractPosAndBoard $ applyContinous board state action False

-- print board properly
applyPlayActions :: [Action] -> (Int, Int) -> [[Tile]] -> IO ()
applyPlayActions (action : actions) state board = do
  let ((newX, newY), newBoard) = applyPlayAction action state board

  if (newX, newY) == state
    then do
      putStrLn ("Sorry, error: cannot move to the " ++ show action)
      putStrLn "Your Current Board: "
      putStrLn (boardString $ boardWithBall board state)
    else do
      putStrLn (boardString $ boardWithBall newBoard (newX, newY))
      if null (findBonuses (enumerator newBoard)) && (board !! newX) !! newY == Target
        then putStrLn "Congratulations! You win the game!"
        else (if length (findBonuses (enumerator newBoard)) < length (findBonuses (enumerator board)) then putStrLn "Collected a bonus!" else putStrLn "")
      if not (null actions) then applyPlayActions actions (newX, newY) newBoard else putStrLn ""

game :: [Char] -> IO ()
game fileName = do
  fileContent <- getFile fileName
  let board = makeBoard fileContent
  putStrLn (boardString board)
  putStrLn "Board Loaded Successfully!"
  input <- getLine
  if take 4 input == "play"
    then
      ( do
          let args = words input
          if length args < 2
            then
              ( do
                  actions <- createActionList []
                  let parsedActions = map parseAction actions
                  -- check if Actions are valid
                  applyPlayActions parsedActions (findBall (enumerator board)) board
              )
            else
              ( putStrLn "Play with Function"
              )
      )
    else
      ( if take 4 input == "quit"
          then do putStrLn "Game Over"
          else
            ( if take 5 input == "solve"
                then putStrLn "Solving"
                else putStrLn "Invalid Input!"
            )
      )

main :: IO ()
main = do
  putStrLn "Enter File Name: "
  fileName <- getLine
  board <- load fileName
  putStrLn "Board loaded successfully!"
  solvable <- check board
  putStrLn ("Solvable: " ++ show solvable)
  let solution = solve board
  putStrLn ("Solution: " ++ show solution)

mainGreedy :: IO ()
mainGreedy = do
  putStrLn "Enter File Name: "
  fileName <- getLine
  board <- load fileName
  putStrLn "Board loaded successfully!"
  solvable <- check board
  putStrLn ("Solvable: " ++ show solvable)
  findSolution board (findBall (enumerator board)) []
