module CheckUtils where

import DataTypes
import Prelude hiding (Left, Right)

actions = [Left, Up, Down, Right]

dontMove :: Board -> Position -> Position -> Bool
dontMove board (x, y) (xNew, yNew) = ((board !! xNew) !! yNew == Grass) || ((board !! x) !! y == Target)

applyAction :: Board -> Position -> Action -> (Position, Board)
applyAction board (x, y) Up =
  if (x - 1 < 0) || (dontMove board (x, y) (x -1, y))
    then ((x, y), board)
    else applyAction board (x - 1, y) Up
applyAction board (x, y) Down =
  if x + 1 >= length board || (dontMove board (x, y) (x + 1, y))
    then ((x, y), board)
    else applyAction board (x + 1, y) Down
applyAction board (x, y) Left =
  if y - 1 < 0 || (dontMove board (x, y) (x, y -1))
    then ((x, y), board)
    else applyAction board (x, y - 1) Left
applyAction board (x, y) Right =
  if y + 1 >= length (head board) || (dontMove board (x, y) (x, y + 1))
    then ((x, y), board)
    else applyAction board (x, y + 1) Right

validAction :: Board -> Position -> Action -> Bool
validAction _ (x, _) Up = (x - 1) >= 0
validAction _ (_, y) Left = (y - 1) >= 0
validAction board (_, y) Right = (y + 1) < length (head board)
validAction board (x, _) Down = (x + 1) < length board

getSuccessors :: Board -> Position -> [Position] -> [Position]
getSuccessors board state visited = [fst $ applyAction board state action | action <- actions, (fst $ applyAction board state action) `notElem` visited]

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