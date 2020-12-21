module ParsePathUtils where

import Data.Function (on)
import Data.List (maximumBy, nub)
import Data.Maybe (fromJust, isNothing)
import DataTypes

-- Removes redundant conditional actions from list of actions
parsePathConditions :: [(Position, Action)] -> Bool -> Char -> Action -> Board -> [(Position, Action)]
parsePathConditions [] _ _ _ _ = []
parsePathConditions (((x, y), action) : remPath) prevCondition color prevAction board
  | addCondition = ((x, y), Cond color action) : parsedPath
  | not prevCondition = ((x, y), action) : parsedPath
  | otherwise = parsedPath
  where
    addCondition = prevCondition && (action /= prevAction)
    nextCond = (board !! x) !! y `elem` [Condition 'p', Condition 'y', Condition 'o']
    (Condition nextColor) = if nextCond then (board !! x) !! y else Condition 'n'
    parsedPath = parsePathConditions remPath nextCond nextColor action board

-- Returns Number of Loops two given actions can form in a list of actions
loopAccumulator :: (Action, Action) -> [Action] -> Int -> (Int, [Action])
loopAccumulator (action1, action2) stack count
  | length stack < 2 || count > 4 = (count, stack)
  | otherwise = if head stack == action1 && stack !! 1 == action2 then loopAccumulator (action1, action2) (drop 2 stack) (count + 1) else (count, stack)

-- Creates Loops in a list of actions
createLoops :: [Action] -> [Action]
createLoops stack
  | length stack < 4 = stack
  | otherwise = if action1 == (stack !! 2) && action2 == (stack !! 3) then result else head stack : createLoops (tail stack)
  where
    action1 = head stack
    action2 = stack !! 1
    (loopCount, remList) = loopAccumulator (action1, action2) (drop 4 stack) 2
    result = LOOP (action1, action2) loopCount : createLoops remList

-- Creates all possible combinations of functions from consecutively occuring elements in a list of actions
createFunctions :: [Action] -> [Action]
createFunctions (LOOP (a1, a2) x1 : b : c : remList) = LOOP (a1, a2) x1 : createFunctions (b : c : remList)
createFunctions (b : LOOP (a1, a2) x1 : c : remList) = [b, LOOP (a1, a2) x1] ++ createFunctions (c : remList)
createFunctions (b : c : LOOP (a1, a2) x1 : remList) = [b, c, LOOP (a1, a2) x1] ++ createFunctions remList
createFunctions (a : b : c : remList) = Function (a, b, c) : createFunctions (b : c : remList)
createFunctions x = x

-- Returns only Functions from a given list of actions
extractFunctions :: [Action] -> [Action]
extractFunctions [] = []
extractFunctions (Function (a, b, c) : remList) = Function (a, b, c) : extractFunctions remList
extractFunctions (x : remList) = extractFunctions remList

-- Returns the most frequently occuring function from a list of functions
mostFreqFunction :: [Action] -> Action
mostFreqFunction list = fst $ maximumBy (compare `on` snd) elemCounts
  where
    elemCounts = nub [(element, count) | element <- list, let count = length (filter (== element) list)]

-- Replaces all simaltaneous occurences of the actions in the function in a list of actions by the described function
replaceWithFunction :: [Action] -> Action -> [Action]
replaceWithFunction actions fn
  | length actions < 3 = actions
  | otherwise = if head actions == a1 && actions !! 1 == a2 && actions !! 2 == a3 then fn : replaceWithFunction (drop 3 actions) fn else head actions : replaceWithFunction (tail actions) fn
  where
    Function (a1, a2, a3) = fn

-- Creates Function actions in a list of actions
addFunctionToPath :: [Action] -> [Action]
addFunctionToPath actions = if null functions then actions else replaceWithFunction actions (mostFreqFunction functions)
  where
    functions = extractFunctions $ createFunctions actions

-- Condenses path to include Loops and Functions
parsePath :: [(Position, Action)] -> Board -> [Action]
parsePath optimalPath board = finalPathWithFunctions
  where
    parsedWithCond = parsePathConditions optimalPath False 'n' START board
    pathWithOnlyActions = [action | (_, action) <- parsedWithCond]
    parsedWithLoops = createLoops (tail pathWithOnlyActions) -- remove START
    finalPathWithFunctions = addFunctionToPath parsedWithLoops

-- Returns a function if list of actions contains function otherwise Nothing
findFunction :: [Action] -> Maybe Action
findFunction [] = Nothing
findFunction (Function (a1, a2, a3) : xs) = Just (Function (a1, a2, a3))
findFunction (_ : xs) = findFunction xs

-- Returns whether given action is a function
isFunction :: Action -> Bool
isFunction (Function (_, _, _)) = True
isFunction _ = False

-- Returns string representation of the solution (a list of actions)
stringifyPath :: [Action] -> String
stringifyPath path = if isNothing functionInPath then unwords formedStringArray else unwords (formedStringArray ++ [show $ fromJust functionInPath])
  where
    formedStringArray = [if isFunction direction then "Function" else show direction | direction <- path]
    functionInPath = findFunction path