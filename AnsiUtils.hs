module AnsiUtils where

import DataTypes
import System.Console.ANSI

tileToColor :: Tile -> IO ()
tileToColor Star = do
  setSGR [SetColor Foreground Vivid Yellow]
  putStr (show Star ++ " ")
tileToColor Grass = do
  setSGR [SetColor Foreground Vivid Green]
  putStr (show Grass ++ " ")
tileToColor Path = do
  setSGR [SetColor Foreground Dull Blue]
  putStr (show Path ++ " ")
tileToColor (Condition 'p') = do
  setSGR [SetColor Foreground Vivid Magenta]
  putStr ("p" ++ " ")
tileToColor (Condition 'y') = do
  setSGR [SetColor Foreground Vivid Yellow]
  putStr ("y" ++ " ")
tileToColor (Condition 'o') = do
  setSGR [SetColor Foreground Dull Yellow]
  putStr ("o" ++ " ")
tileToColor Target = do
  setSGR [SetColor Foreground Vivid Red]
  putStr (show Target ++ " ")
tileToColor element = do
  setSGR [Reset]
  putStr (show element ++ " ")

colorRow :: [Tile] -> IO ()
colorRow [] = do
  putStr ""
colorRow (x : xs) = do
  tileToColor x
  colorRow xs

printBoard :: Board -> IO ()
printBoard [] = do
  setSGR [Reset]
printBoard (x : xs) = do
  colorRow x
  putStrLn ""
  printBoard xs
