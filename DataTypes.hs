module DataTypes where

import Prelude hiding (Left, Right)

data Tile = Grass | Ball | Condition Char | Star | Path | Target deriving (Eq)

data Action = Up | Down | Right | Left | START | Cond Char Action | LOOP (Action, Action) Int | Function (Action, Action, Action) | Invalid deriving (Eq)

type Board = [[Tile]]

type Position = (Int, Int)

instance Show Action where
  show Up = "Up"
  show Down = "Down"
  show Right = "Right"
  show Left = "Left"
  show (Cond color act) = "Cond{" ++ [color] ++ "}{" ++ show act ++ "}"
  show (LOOP (a1, a2) freq) = "Loop{" ++ show freq ++ "}{" ++ show a1 ++ "," ++ show a2 ++ "}"
  show (Function (a1, a2, a3)) = "with " ++ show a1 ++ " " ++ show a2 ++ " " ++ show a3
  show Invalid = "Invalid"

instance Show Tile where
  show Grass = "*"
  show Path = "-"
  show Star = "b"
  show Ball = "@"
  show (Condition 'p') = "p"
  show (Condition 'o') = "o"
  show (Condition 'y') = "y"
  show Target = "t"