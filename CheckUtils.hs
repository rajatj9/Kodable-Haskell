module CheckUtils where

import DataTypes
import Prelude hiding (Left, Right)

actions = [Left, Up, Down, Right]

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