import AnsiUtils
import BoardUtils
  ( boardString,
    enumerator,
    findBall,
    findBonuses,
    getFile,
    makeBoard,
  )
import DataTypes (Board)
import Kodable (applyPlayActions, createActionList, solve)
import MapGenerator
import ParsePathUtils (stringifyPath)

-- Menu Function which controls the flow of control
game :: Board -> IO ()
game board = do
  if board == [] then putStrLn "Welcome to the Game!\n" else putStrLn ""
  putStrLn "Options Available:"
  putStrLn "1. Load a board -> load \"filename\""
  putStrLn "2. Check if loaded board is solvable -> check"
  putStrLn "3. Play on current board -> play or play Right Up Down to specify function arguments."
  putStrLn "4. Find optimal solution for current board -> solve"
  putStrLn "5. Generate a new board -> generate"
  putStrLn "6. Quit the game -> quit\n"
  input <- getLine
  if take 4 input == "load"
    then
      ( do
          let fileName = init $ tail $ ((words input) !! 1)
          fileContent <- getFile fileName
          if not (null fileContent)
            then do
              let board = makeBoard fileContent
              printBoard board
              putStrLn "Board Loaded Successfully!\n"
              game board
            else game []
      )
    else
      ( if take 4 input == "play"
          then
            ( if null board
                then do
                  putStrLn "Please load a board first!"
                  game []
                else do
                  let args = words input
                  if length args < 2
                    then
                      ( do
                          putStrLn "Type 'Hint' for asking for hints at any stage"
                          actions <- createActionList [] board []
                          applyPlayActions actions (findBall (enumerator board)) board (length $ findBonuses $ enumerator board)
                      )
                    else
                      ( do
                          putStrLn "Type 'Hint' for asking for hints at any stage"
                          let fnActions = tail (words input)
                          actions <- createActionList [] board fnActions
                          applyPlayActions actions (findBall (enumerator board)) board (length $ findBonuses $ enumerator board)
                      )
                  game board
            )
          else
            ( if take 4 input == "quit"
                then do
                  putStrLn "\nGame Over! Thanks for playing!"
                else
                  ( if take 5 input == "solve" || take 5 input == "check"
                      then
                        if null board
                          then do
                            putStrLn "Please load a board first!"
                            game []
                          else do
                            let solution = solve board
                            if null solution
                              then do
                                putStrLn "Board not solvable!"
                                game board
                              else do
                                if (take 5 input) == "solve" then putStrLn ("Solution: " ++ stringifyPath solution) else putStrLn ("Solvable: " ++ show (not (null solution)))
                                game board
                      else
                        ( if take 8 input == "generate"
                            then do
                              genBoard
                              game board
                            else do
                              putStrLn "Invalid Input!"
                              game board
                        )
                  )
            )
      )

main :: IO ()
main = game []
