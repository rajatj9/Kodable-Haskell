module CheckUtils where

import DataTypes
import Prelude hiding (Left, Right)

-- List of posssible actions
actions :: [Action]
actions = [Left, Up, Down, Right]

-- Checks if an action is a valid action on a given state
validAction :: Board -> Position -> Action -> Bool
validAction _ (x, _) Up = (x - 1) >= 0
validAction _ (_, y) Left = (y - 1) >= 0
validAction board (_, y) Right = (y + 1) < length (head board)
validAction board (x, _) Down = (x + 1) < length board
