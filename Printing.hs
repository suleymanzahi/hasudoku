module Printing where

import Core

-- function for removing the delimiter string from the files
removeDelimiter :: [String] -> [String]
removeDelimiter = filter (\s -> head s /= '=')

-- function for producing complete sudoku strings from the files
-- input is a list containing all lines from all sudokus, so 9 or 4 lines form a sudoku
-- we take 9(or 4) lines and make a string, doing so recursively for the rest
parseSudokuStrings :: [String] -> [String]
parseSudokuStrings [] = []
parseSudokuStrings xs = concat (take 9 xs) : parseSudokuStrings (drop 9 xs)

-- prints out the menu
menu :: IO ()
menu = do
  putStrLn "1. Automatically solve and print result"
  putStrLn "2. Solve all Sudokus starting from the current one"
  putStrLn "3. Try to solve the chosen sudoku"
  putStrLn "4. Quit program"

-- we read the contents of the file
-- using lines, we get a list of lines, from which we remove the delimiter strings
-- we then map verifySudoku to every sudoku string, created from parseSudokuStrings

-- function fo adding the horizontal bar for printing
-- we insert the bar for every third element in the list
addBar :: String -> String
addBar xs = if length xs <= 3 then xs else take 3 xs ++ "|" ++ addBar (drop 3 xs)

horizontal :: String
horizontal = "------+-------+------"

--same as addBar, but used when we split up a Sudoku string to a list of 9 substrings(each row)
-- then the horizontal line is added for every third element.
addHorizontal :: [String] -> [String]
addHorizontal xs = if length xs <= 3 then xs else take 3 xs ++ [horizontal] ++ addHorizontal (drop 3 xs)

-- function for adding space between characters in a string
-- recursively, add space between first non-visited character and the rest
space :: String -> String
space [] = []
space [x] = [x]
space (x : xs) = x : ' ' : space xs

-- since string is a list of chars, we map the lambda function on every character
-- if an char is equal to '.', we return a '0', else we return the same value
replacePointsWithZero :: String -> String
replacePointsWithZero = map (\i -> (if i == '.' then '0' else i))

-- function used for parsing a string as a Board, used for when we keep track of the sudoku
parseInitBoard :: String -> [(String, [Int])]
parseInitBoard s = zip squares (map (: []) str)
  where
    str = map digToInt $ replacePointsWithZero s

-- function to convert a board to a string, used for printing
boardString :: Board -> String
boardString = concatMap (show . head . snd) 

-- function for pretty printing, we split up a sudok string in to a lsit with 9 substrings
-- then add the horizontal line, then we print each substring
-- if it's a horizontal line, just print normally
-- otherwise add some spacing and a vertical bars for each substring
prettyPrint :: String -> IO ()
prettyPrint s = do
  let t = addHorizontal $ chunks 9 s
  putStrLn "---------------------"
  mapM_ (\s -> if head s == '-' then putStrLn s else putStrLn $ space $ addBar s) t
  putStrLn "---------------------"


