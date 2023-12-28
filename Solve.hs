module Solve where

import Core
import Printing

-- pattern match with the option chosen by the user to call the appropriate function
executeInput :: String -> Int -> String -> [String] -> IO ()
executeInput "1" _ s _ = solveSingleSudoku s
executeInput "2" _ s xs = solveAllSudokus xs
executeInput "3" _ s _ = userSolveSudoku s
executeInput "4" _ _ _ = do putStrLn "Bye..."; return ()
executeInput _ _ _ _ = putStrLn "Invalid option. Try again"

-- function for solving a single sudoku
-- we solve the suduku with solveSudoku, extract the board and pass the string to prettyPrint
solveSingleSudoku :: String -> IO ()
solveSingleSudoku s = do
  prettyPrint $ boardString (fromMaybe [] $ solveSudoku s)

-- similar to the function above, but now we solve several sudokus
solveAllSudokus :: [String] -> IO ()
solveAllSudokus = mapM_ solveSingleSudoku

-- main function for where the user tries to solve a sudoku
-- we have essentially 3 sudukos,
-- trackBoard for keeping track of the real sudoku that should be printed
-- board, where we keep track of what values are okay to assign
-- solvedSudoku, the solved sudoku which we compare with trackBoard to see 
-- if the user has found the solution. we then call a helper function
userSolveSudoku :: String -> IO ()
userSolveSudoku s = do
  let trackBoard = parseInitBoard s
  let board = parseBoard s
  let solvedSudoku = solveSudoku s
  prettyPrint $ boardString $ parseInitBoard s
  userSolveSudoku' board solvedSudoku trackBoard

-- we start of by asking the user for what value should be assigned to which square
-- if the value exists in the list of valid values in the parsed board
-- then we can proceed, otherwise it's a invalid move and the user is prompted again
-- for the main loop, we check if the value assigned doesn't return a Nothing board
-- i.e no square returns a empty list, which is a invalid move. if it happens, we prompt
-- the user again. if it's assignable, then we uppdate the trackBoard and check if it's equal
-- to the solved sudoku. if it is, we are done and return to the menu, otherwise we keep on going
-- till the sudoku is solved  
userSolveSudoku' :: Maybe Board -> Maybe Board -> Board -> IO ()
userSolveSudoku' b fb tb = do
  putStrLn "Enter a square, e.g A1"
  s <- getLine
  putStrLn "Enter a value to insert"
  x <- getLine
  let v = read x :: Int
  if v `elem` lookupList s (fromMaybe [] b)
    then do
      let updatedBoard = assign v s (fromMaybe [] b)
      if isNothing updatedBoard then do
        putStrLn "Invalid move. Try again"
        userSolveSudoku' b fb tb else do
          let updateTb = setValue v s tb
          if updateTb == fromMaybe [] fb
            then putStrLn "Finished sudoku"
            else do
              putStrLn "Great! Continue"
              prettyPrint $ boardString updateTb
              userSolveSudoku' updatedBoard fb updateTb
    else do
      putStrLn "Invalid move. Try again"
      userSolveSudoku' b fb tb

-- helper function for the game loop
-- we start by prompting the user select a sudoku between 1 and len
-- where len is the amount of sudokus loaded in, which here is a list of sudoku strings
-- if the user's choice is in range, we print the options
-- we then create the data needed for the other functions
-- first, we extract the sudoku string the user selected
-- then we drop all sudoku strings preceding the selected sudoku from our list of sudokus
-- then all that data is passed to executeInput
mainHelp :: Int -> [String] -> IO ()
mainHelp len xs = do
  putStrLn ("To select a Sudoku, choose a number between 1 - " ++ show len)
  i <- getLine
  let n = read i :: Int
  if n `elem` [1 .. len]
    then putStrLn ("You chose sudoku no. " ++ show n ++ ", here's your options")
    else putStrLn "Number not in range. Try again"
  let s = xs !! (n - 1)
  menu
  opt <- getLine
  if opt == "4"
    then do
      putStrLn "Bye..."
      return ()
    else do
      executeInput opt len s $ drop (n - 1) xs
      mainHelp len xs

-- in the main function, we prompt the the user for the file name
-- then we create a list containing sudoku strings, which is then 
-- passed on to the game loop function mainHelper
main :: IO ()
main = do
  putStrLn "Enter file name:"
  fileName <- getLine
  contents <- readFile fileName
  let partialSudokus = removeDelimiter $ lines contents
  let completeSudokus = parseSudokuStrings partialSudokus
  let len = length completeSudokus
  putStrLn ("A file with " ++ show len ++ " Sudokus has been loaded in.")
  mainHelp len completeSudokus

test1Incomplete = "003020600900305001001806400008102900700000008006708200002609500800203009005010300"

test1AlmostComplete = "483921657967345821251876493548132976729564138136798245372689514814253769695417000"

test2Incomplete = "000000907000420180000705026100904000050000040000507009920108000034059000507000000"

test2AlmostComplete = "462831957095426183381795426173904265659012748248567319926178534834259671517643892"
