module Kodable where

import ActionParser
import Data.Char
import Data.Function (on)
import Data.List (maximumBy, minimumBy, nub)
import Data.Ord (comparing)
import DataTypes
import System.Directory (doesFileExist)
import System.IO ()
import Prelude hiding (Left, Right)

actions :: [Action]
actions = [Left, Up, Down, Right]

colors :: [Char]
colors = ['o', 'p', 'y']

conditions :: [Action]
conditions = [Cond color action | color <- colors, action <- actions]

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

load :: String -> IO Board
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

applyAction :: Board -> Position -> Action -> Position
applyAction board (x, y) Up = if ((board !! (x - 1)) !! y) /= Grass then (x - 1, y) else (-1, -1)
applyAction board (x, y) Down = if ((board !! (x + 1)) !! y) /= Grass then (x + 1, y) else (-1, -1)
applyAction board (x, y) Left = if ((board !! x) !! (y - 1)) /= Grass then (x, y - 1) else (-1, -1)
applyAction board (x, y) Right = if ((board !! x) !! (y + 1)) /= Grass then (x, y + 1) else (-1, -1)

validAction :: Board -> Position -> Action -> Bool
validAction _ (x, _) Up = (x - 1) >= 0
validAction _ (_, y) Left = (y - 1) >= 0
validAction board (_, y) Right = (y + 1) < length (head board)
validAction board (x, _) Down = (x + 1) < length board

getSuccessors :: Board -> Position -> [Position] -> [Position]
getSuccessors board state visited = [applyAction board state action | action <- actions, validAction board state action, applyAction board state action /= (-1, -1) && not (elem (applyAction board state action) visited)]

dfs :: Board -> [[Position]] -> [Position] -> IO Bool
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

check :: Board -> IO Bool
check board = dfs board stack visited
  where
    stack = [[findBall (enumerator board)]]
    visited = []

-- Solver

blockNodes :: [Tile]
blockNodes = [Condition 'p', Condition 'y', Condition 'o']

isBlockNode :: Board -> Position -> Bool
isBlockNode board (x, y) = ((board !! x) !! y) `elem` blockNodes

removeStar :: Board -> Position -> Board
removeStar board (x, y) = [[if x1 == x && y == y1 && tile == Star then Path else tile | (y1, tile) <- enumerate row] | (x1, row) <- enumerate board]

notMove :: Board -> Position -> Position -> Int -> Bool
notMove board (x, y) (xNew, yNew) numBonus = ((board !! x !! y) `elem` blockNodes) || ((board !! xNew) !! yNew == Grass) || ((board !! x) !! y == Target)

newBonus :: Num p => Board -> Position -> p -> p
newBonus board (x, y) prevBonus = if (board !! x) !! y == Star then prevBonus - 1 else prevBonus

applyContinous :: Board -> Position -> Action -> Bool -> Int -> (Position, Action, Board, Int)
applyContinous board (x, y) Up forced numBonus =
  if (x - 1 < 0) || (notMove board (x, y) (x -1, y) numBonus && not forced)
    then ((x, y), Up, board, numBonus)
    else applyContinous (removeStar board (x, y)) (x - 1, y) Up False (newBonus board (x, y) numBonus)
applyContinous board (x, y) Down forced numBonus =
  if x + 1 >= length board || (notMove board (x, y) (x + 1, y) numBonus && not forced)
    then ((x, y), Down, board, numBonus)
    else applyContinous (removeStar board (x, y)) (x + 1, y) Down False (newBonus board (x, y) numBonus)
applyContinous board (x, y) Left forced numBonus =
  if y - 1 < 0 || (notMove board (x, y) (x, y -1) numBonus && not forced)
    then ((x, y), Left, board, numBonus)
    else applyContinous (removeStar board (x, y)) (x, y - 1) Left False (newBonus board (x, y) numBonus)
applyContinous board (x, y) Right forced numBonus =
  if y + 1 >= length (head board) || (notMove board (x, y) (x, y + 1) numBonus && not forced)
    then ((x, y), Right, board, numBonus)
    else applyContinous (removeStar board (x, y)) (x, y + 1) Right False (newBonus board (x, y) numBonus)

getStateAndBonuses :: (Position, Action, Board, Int) -> (Position, Int)
getStateAndBonuses (pos, action, board, numBonus) = (pos, numBonus)

getSolverSuccessorsBFS :: Board -> Position -> [(Position, Int)] -> Int -> [(Position, Action, Board, Int)]
getSolverSuccessorsBFS board state visited numBonus = [applyContinous board state action (isBlockNode board state) numBonus | action <- actions, validAction board state action, getStateAndBonuses (applyContinous board state action (isBlockNode board state) numBonus) `notElem` visited]

exploreNeighbours :: ([(Position, Action)], [(Position, Int)], Board, Int) -> [([(Position, Action)], [(Position, Int)], Board, Int)]
exploreNeighbours frontier
  | ((board !! x) !! y) == Target = [frontier]
  | null successors = []
  | otherwise =
    [ ( path ++ [(newState, newAction)],
        visited ++ [(newState, newBonus)],
        newBoard,
        newBonus
      )
      | (newState, newAction, newBoard, newBonus) <- successors
    ]
  where
    (path, visited, board, bonusCount) = frontier
    (state, lastAction) = last path
    (x, y) = state
    successors = getSolverSuccessorsBFS board state visited bonusCount

findPathsBFS :: [([(Position, Action)], [(Position, Int)], Board, Int)] -> [([(Position, Action)], [(Position, Int)], Board, Int)]
findPathsBFS stack
  | stack == newStack = stack -- All frontiers have reached Target (no new neighbours)
  | otherwise = findPathsBFS newStack
  where
    newStack = concat [exploreNeighbours frontier | frontier <- stack]

findCompletePaths :: Board -> [[(Position, Action)]]
findCompletePaths board = extractCompletePaths $ findPathsBFS [([(ballPos, START)], [(ballPos, initBonus)], board, (length $ findBonuses $ enumerator board))]
  where
    ballPos = findBall (enumerator board)
    initBonus = length (findBonuses $ enumerator board)

findUnCollectableBonuses :: [([(Position, Action)], [(Position, Int)], Board, Int)] -> Int
findUnCollectableBonuses stack = minimum [numBonus | (path, visited, board, numBonus) <- stack]

extractCompletePaths :: [([(Position, Action)], [(Position, Int)], Board, Int)] -> [[(Position, Action)]]
extractCompletePaths stack = [path | (path, visited, _, numBonus) <- stack, numBonus == unCollectableBonuses]
  where
    unCollectableBonuses = findUnCollectableBonuses stack

findOptimalPath :: [[(Position, Action)]] -> Board -> [Action]
findOptimalPath path board = parsePath (minimumBy (comparing length) path) board

findOptimalPathConsiderLoopsAndFuncs :: [[(Position, Action)]] -> Board -> [Action]
findOptimalPathConsiderLoopsAndFuncs path board = minimumBy (comparing length) (map (\p -> parsePath p board) path)

parsePathConditions :: [(Position, Action)] -> Bool -> Char -> Action -> Board -> [(Position, Action)]
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
createFunctions (LOOP (a1, a2) x1 : b : c : remList) = LOOP (a1, a2) x1 : createFunctions (b : c : remList)
createFunctions (b : LOOP (a1, a2) x1 : c : remList) = [b, LOOP (a1, a2) x1] ++ createFunctions (c : remList)
createFunctions (b : c : LOOP (a1, a2) x1 : remList) = [b, c, LOOP (a1, a2) x1] ++ createFunctions remList
createFunctions (a : b : c : remList) = Function (a, b, c) : createFunctions (b : c : remList)
createFunctions x = x

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

parsePath :: [(Position, Action)] -> Board -> [Action]
parsePath optimalPath board = finalPathWithFunctions
  where
    parsedWithCond = parsePathConditions optimalPath False 'n' START board
    pathWithOnlyActions = [action | (_, action) <- parsedWithCond]
    parsedWithLoops = createLoops (tail pathWithOnlyActions) -- remove START
    finalPathWithFunctions = addFunctionToPath parsedWithLoops

solve :: Board -> [Action]
solve board = findOptimalPathConsiderLoopsAndFuncs completePaths board
  where
    completePaths = findCompletePaths board

createActionList :: [Action] -> Board -> IO [Action]
createActionList actions board = do
  if null actions then putStr "First Direction: " else putStr "Next Direction: "
  direction <- getLine
  if direction == ""
    then return actions
    else
      ( if direction == "Hint"
          then do
            let (state, newBoard, newBonus) = applyPlayActionsNonIO actions (findBall (enumerator board)) board (length $ findBonuses $ enumerator board)
            let hint = head (solve (boardWithBall newBoard state))
            putStrLn ("Hint: " ++ show hint)
            createActionList actions newBoard
          else do
            let action = parseAction direction
            if action /= Invalid
              then createActionList (actions ++ [parseAction direction]) board
              else do
                putStrLn "Invalid Action!"
                createActionList actions board
      )

createActionListWithFunction :: [Action] -> Board -> [String] -> IO [Action]
createActionListWithFunction actions board funcStr = do
  if null actions then putStr "First Direction: " else putStr "Next Direction: "
  direction <- getLine
  if direction == "Function"
    then do createActionListWithFunction (actions ++ [parseFunction funcStr]) board funcStr
    else
      ( if direction == ""
          then return actions
          else
            ( if direction == "Hint"
                then do
                  let (state, newBoard, newBonus) = applyPlayActionsNonIO actions (findBall (enumerator board)) board (length $ findBonuses $ enumerator board)
                  let hint = head (solve (boardWithBall newBoard state))
                  putStrLn ("Hint: " ++ show hint)
                  createActionList actions newBoard
                else
                  ( do
                      let action = parseAction direction
                      if action /= Invalid
                        then createActionListWithFunction (actions ++ [parseAction direction]) board funcStr
                        else do
                          putStrLn "Invalid Action"
                          createActionListWithFunction actions board funcStr
                  )
            )
      )

seperateByComma :: [Char] -> [[Char]]
seperateByComma str = words [if c == ',' then ' ' else c | c <- str]

parseFunction :: [String] -> Action
parseFunction [a1, a2, a3] = Function (parseAction a1, parseAction a2, parseAction a3)

parseActionWrapper :: [Char] -> IO Action
parseActionWrapper actStr = do
  let action = parseAction actStr
  return action

applyLoop :: Board -> Position -> Int -> Int -> (Action, Action) -> (Position, Board, Int)
applyLoop board state numBonus loopFreq (action1, action2)
  | loopFreq <= 0 = (state, board, numBonus)
  | otherwise = applyLoop finalBoard finalState finalBonus (loopFreq - 1) (action1, action2)
  where
    (newState, newBoard, newBonus) = applyPlayAction action1 state board numBonus
    (finalState, finalBoard, finalBonus) = applyPlayAction action2 newState newBoard newBonus

applyFunction :: Board -> Position -> Int -> (Action, Action, Action) -> (Position, Board, Int)
applyFunction board state numBonus (action1, action2, action3) = applyPlayAction action3 state2 board2 bonus2
  where
    (state1, board1, bonus1) = applyPlayAction action1 state board numBonus
    (state2, board2, bonus2) = applyPlayAction action2 state1 board1 bonus1

extractPosAndBoard :: (a, b1, b2, b3) -> (a, b2, b3)
extractPosAndBoard (pos, _, board, numBonus) = (pos, board, numBonus)

applyPlayAction :: Action -> Position -> Board -> Int -> (Position, Board, Int)
applyPlayAction (LOOP (a1, a2) loopFreq) state board numBonus = applyLoop board state numBonus loopFreq (a1, a2)
applyPlayAction (Cond color action) state board numBonus = extractPosAndBoard $ applyContinous board state action True numBonus
applyPlayAction (Function (a1, a2, a3)) state board numBonus = applyFunction board state numBonus (a1, a2, a3)
applyPlayAction action state board numBonus = extractPosAndBoard $ applyContinous board state action False numBonus

isLoopAction :: Action -> Bool
isLoopAction (LOOP (_, _) _) = True
isLoopAction _ = False

isFunctionAction :: Action -> Bool
isFunctionAction (Function (_, _, _)) = True
isFunctionAction _ = False

extractLastAction :: Action -> Action
extractLastAction (LOOP (_, action) _) = action
extractLastAction (Function (_, _, action)) = action

-- print board properly
applyPlayActions :: [Action] -> Position -> Board -> Int -> IO ()
applyPlayActions (action : actions) state board numBonus = do
  let ((newX, newY), newBoard, newBonus) = applyPlayAction action state board numBonus
  -- if we reach a condition tile and the next action is not a Conditional action then keep moving
  if (board !! newX) !! newY `elem` blockNodes && not (null actions) && (head actions `notElem` conditions)
    then do
      let (Condition color) = (board !! newX) !! newY
      let extractedAction = if isLoopAction action || isFunctionAction action then extractLastAction action else action
      let newAction = Cond color extractedAction
      applyPlayActions (newAction : actions) (newX, newY) newBoard newBonus
    else
      ( if (newX, newY) == state
          then do
            putStrLn ("Sorry, error: cannot move to the " ++ show action)
            putStrLn "Your Current Board: "
            putStrLn (boardString $ boardWithBall board state)
          else do
            putStrLn (boardString $ boardWithBall newBoard (newX, newY))
            if (board !! newX) !! newY == Target
              then putStrLn ("Congratulations! You win the game " ++ "with " ++ show (newBonus) ++ "remaining bonuses.")
              else (if newBonus < numBonus then putStrLn "Collected a bonus!" else putStrLn "")
            if not (null actions) then applyPlayActions actions (newX, newY) newBoard newBonus else putStrLn ""
      )

applyPlayActionsNonIO :: [Action] -> Position -> Board -> Int -> (Position, Board, Int)
applyPlayActionsNonIO [] state board numBonus = (state, board, numBonus)
applyPlayActionsNonIO (action : actions) state board numBonus = do
  let ((newX, newY), newBoard, newBonus) = applyPlayAction action state board numBonus
  if (board !! newX) !! newY `elem` blockNodes && not (null actions) && (head actions `notElem` conditions)
    then do
      let (Condition color) = (board !! newX) !! newY
      let extractedAction = if isLoopAction action || isFunctionAction action then extractLastAction action else action
      let newAction = Cond color extractedAction
      applyPlayActionsNonIO (newAction : actions) (newX, newY) newBoard newBonus
    else
      ( if (newX, newY) == state
          then ((newX, newY), newBoard, newBonus)
          else do
            ( if ( null (findBonuses (enumerator newBoard))
                     && (board !! newX) !! newY == Target
                 )
                || null actions
                then ((newX, newY), newBoard, newBonus)
                else applyPlayActionsNonIO actions (newX, newY) newBoard newBonus
              )
      )

game :: Board -> IO ()
game board = do
  if board == [] then putStrLn "Welcome to the Game!\n" else putStrLn ""
  putStrLn "Options Available:"
  putStrLn "1. Load a board -> load \"filename\""
  putStrLn "2. Check if loaded board is solvable -> check"
  putStrLn "3. Play on current board -> play or play Right Up Down to specify function arguments."
  putStrLn "4. Find optimal solution for current board -> solve"
  putStrLn "5. Quit the game -> quit\n"
  input <- getLine
  if take 4 input == "load"
    then
      ( do
          let fileName = init $ tail $ ((words input) !! 1)
          fileContent <- getFile fileName
          if not (null fileContent)
            then do
              let board = makeBoard fileContent
              putStrLn (boardString board)
              putStrLn "Board Loaded Successfully!\n"
              game board
            else game []
      )
    else
      ( if take 4 input == "play"
          then
            ( if null board
                then do
                  putStrLn "Please load a board first!"
                  game []
                else do
                  let args = words input
                  if length args < 2
                    then
                      ( do
                          putStrLn "Type 'Hint' for asking for hints at any stage"
                          actions <- createActionList [] board
                          applyPlayActions actions (findBall (enumerator board)) board (length $ findBonuses $ enumerator board)
                          game board
                      )
                    else
                      ( do
                          putStrLn "Type 'Hint' for asking for hints at any stage"
                          let fnActions = tail (words input)
                          actions <- createActionListWithFunction [] board fnActions
                          applyPlayActions actions (findBall (enumerator board)) board (length $ findBonuses $ enumerator board)
                          game board
                      )
            )
          else
            ( if take 4 input == "quit"
                then do
                  putStrLn "\nGame Over! Thanks for playing!"
                else
                  ( if take 5 input == "solve"
                      then
                        if null board
                          then do
                            putStrLn "Please load a board first!"
                            game []
                          else do
                            res <- check board
                            if not res
                              then do
                                putStrLn "Board not solvable!"
                                game board
                              else do
                                putStrLn "Solving"
                                let solution = solve board
                                putStrLn ("Solution: " ++ (unwords $ map show solution))
                                game board
                      else
                        ( if take 5 input == "check"
                            then
                              if null board
                                then do
                                  putStrLn "Please load a board first!"
                                  game []
                                else do
                                  res <- check board
                                  putStrLn ("Solvable: " ++ show res)
                                  game board
                            else do
                              putStrLn "Invalid Input!"
                              game board
                        )
                  )
            )
      )

main :: IO ()
main = game []
