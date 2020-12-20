module PlayUtils where

import ActionParser
import DataTypes

seperateByComma :: [Char] -> [[Char]]
seperateByComma str = words [if c == ',' then ' ' else c | c <- str]

parseFunction :: [String] -> Action
parseFunction [a1, a2, a3] = Function (parseAction a1, parseAction a2, parseAction a3)

extractPosAndBoard :: (a, b1, b2, b3) -> (a, b2, b3)
extractPosAndBoard (pos, _, board, numBonus) = (pos, board, numBonus)

isLoopAction :: Action -> Bool
isLoopAction (LOOP (_, _) _) = True
isLoopAction _ = False

isFunctionAction :: Action -> Bool
isFunctionAction (Function (_, _, _)) = True
isFunctionAction _ = False

extractLastAction :: Action -> Action
extractLastAction (LOOP (_, action) _) = action
extractLastAction (Function (_, _, action)) = action
