module PlayUtils where

import ActionParser
import DataTypes

-- Splits string by a comma
seperateByComma :: [Char] -> [[Char]]
seperateByComma str = words [if c == ',' then ' ' else c | c <- str]

-- Creates Function from a list of string representation of Actions
parseFunction :: [String] -> Action
parseFunction [a1, a2, a3] = Function (parseAction a1, parseAction a2, parseAction a3)

-- Extracts Position, Board, and remaining bonuses from the state
extractPosAndBoard :: (a, b1, b2, b3) -> (a, b2, b3)
extractPosAndBoard (pos, _, board, numBonus) = (pos, board, numBonus)

-- Determines whether the given action is a Loop
isLoopAction :: Action -> Bool
isLoopAction (LOOP (_, _) _) = True
isLoopAction _ = False

-- Determines whether a given action is a Function
isFunctionAction :: Action -> Bool
isFunctionAction (Function (_, _, _)) = True
isFunctionAction _ = False

-- Returns last action from a Loop or Function action
extractLastAction :: Action -> Action
extractLastAction (LOOP (_, action) _) = action
extractLastAction (Function (_, _, action)) = action
