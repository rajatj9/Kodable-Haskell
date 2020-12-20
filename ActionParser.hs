module ActionParser where

import DataTypes
import Parser
import Prelude hiding (Left, Right)

conditionParser :: Parser Action
conditionParser = do
  string "Cond{"
  colour <- string "p" +++ string "y" +++ string "o"
  string "}{"
  direction <- string "Down" +++ string "Up" +++ string "Left" +++ string "Right"
  string "}"
  return $ Cond (head colour) (parseAction direction)

loopParser :: Parser Action
loopParser = do
  string "Loop{"
  number <- string "0" +++ string "1" +++ string "2" +++ string "3" +++ string "4" +++ string "5"
  string "}{"
  direction1 <- string "Down" +++ string "Up" +++ string "Left" +++ string "Right" +++ conditionalContainedParser
  string ","
  direction2 <- string "Down" +++ string "Up" +++ string "Left" +++ string "Right" +++ conditionalContainedParser
  string "}"
  return $ LOOP (parseAction direction1, parseAction direction2) (read number :: Int)
  where
    conditionalContainedParser =
      parser
        ( \s -> case runParser conditionParser s of
            [] -> []
            [(x, xs)] -> [(show x, xs)]
        )

parseAction :: String -> Action
parseAction inpString
  | inpString == "Right" = Right
  | inpString == "Left" = Left
  | inpString == "Down" = Down
  | inpString == "Up" = Up
  | runParser conditionParser inpString /= [] = fst (head (runParser conditionParser inpString))
  | runParser loopParser inpString /= [] = fst (head (runParser loopParser inpString))
  | otherwise = Invalid