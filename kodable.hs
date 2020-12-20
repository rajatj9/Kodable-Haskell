module Kodable where

import ActionParser
import BoardUtils
import CheckUtils
import Data.Char
import Data.Foldable (minimumBy)
import Data.Ord (comparing)
import DataTypes
import ParsePathUtils
import PlayUtils
import System.IO ()
import Prelude hiding (Left, Right)

colors :: [Char]
colors = ['o', 'p', 'y']

conditions :: [Action]
conditions = [Cond color action | color <- colors, action <- actions]

load :: String -> IO Board
load fileName = do
  fileContent <- getFile fileName
  return (makeBoard fileContent)

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

solve :: Board -> [Action]
solve board = findOptimalPathConsiderLoopsAndFuncs completePaths board
  where
    completePaths = findCompletePaths board

createActionList :: [Action] -> Board -> [String] -> IO [Action]
createActionList actions board funcStr = do
  if null actions then putStr "First Direction: " else putStr "Next Direction: "
  direction <- getLine
  if direction == "Function"
    then do createActionList (actions ++ [parseFunction funcStr]) board funcStr
    else
      ( if direction == ""
          then return actions
          else
            ( if direction == "Hint"
                then do
                  let (state, newBoard, newBonus) = applyPlayActionsForHint actions (findBall (enumerator board)) board (length $ findBonuses $ enumerator board)
                  let hint = head (solve (boardWithBall newBoard state))
                  putStrLn ("Hint: " ++ show hint)
                  createActionList actions newBoard funcStr
                else
                  ( do
                      let action = parseAction direction
                      if action /= Invalid
                        then createActionList (actions ++ [parseAction direction]) board funcStr
                        else do
                          putStrLn "Invalid Action"
                          createActionList actions board funcStr
                  )
            )
      )

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

applyPlayAction :: Action -> Position -> Board -> Int -> (Position, Board, Int)
applyPlayAction (LOOP (a1, a2) loopFreq) state board numBonus = applyLoop board state numBonus loopFreq (a1, a2)
applyPlayAction (Cond color action) state board numBonus = extractPosAndBoard $ applyContinous board state action True numBonus
applyPlayAction (Function (a1, a2, a3)) state board numBonus = applyFunction board state numBonus (a1, a2, a3)
applyPlayAction action state board numBonus = extractPosAndBoard $ applyContinous board state action False numBonus

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

applyPlayActionsForHint :: [Action] -> Position -> Board -> Int -> (Position, Board, Int)
applyPlayActionsForHint [] state board numBonus = (state, board, numBonus)
applyPlayActionsForHint (action : actions) state board numBonus = do
  let ((newX, newY), newBoard, newBonus) = applyPlayAction action state board numBonus
  if (board !! newX) !! newY `elem` blockNodes && not (null actions) && (head actions `notElem` conditions)
    then do
      let (Condition color) = (board !! newX) !! newY
      let extractedAction = if isLoopAction action || isFunctionAction action then extractLastAction action else action
      let newAction = Cond color extractedAction
      applyPlayActionsForHint (newAction : actions) (newX, newY) newBoard newBonus
    else
      ( if (newX, newY) == state
          then ((newX, newY), newBoard, newBonus)
          else do
            ( if ( null (findBonuses (enumerator newBoard))
                     && (board !! newX) !! newY == Target
                 )
                || null actions
                then ((newX, newY), newBoard, newBonus)
                else applyPlayActionsForHint actions (newX, newY) newBoard newBonus
              )
      )
