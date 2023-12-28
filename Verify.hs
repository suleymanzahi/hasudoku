module Verify where

-- HOW TO RUN
-- With GHC installed, the program can be run by the command runghc VerifySudoku.hs
-- By default the program is set to verify 9*9 sudokus
-- To change it to 4*4, just change the variables below, set sudokuSize to 4 and chunkSize to 2
-- To change the file read, just change the variable fileName below to the desired file name

sudokuSize = 9

chunkSize = 3

-- fileName = "easy50.txt" -- inconsistent20, easy50

rows = take sudokuSize ['A' ..]

cols = take sudokuSize ['1' ..]

-- a and b are our input list, we use list comprehension, we take out an element from a and b, and produce a list
cross :: [a] -> [a] -> [[a]]
cross a b = [[x, y] | x <- a, y <- b]

-- since string is a list of chars, we map the lambda function on every character
-- if an char is equal to '.', we return a '0', else we return the same value
replacePointsWithZero :: String -> String
replacePointsWithZero = map (\i -> (if i == '.' then '0' else i))

-- we use the cross function to combine rows and cols
squareStrings :: [String]
squareStrings = cross rows cols

-- we convert each char to a string, then we use
-- the read function to convert it to a Int
digToInt :: Char -> Int
digToInt c = read [c] :: Int

--our implementation of chunksOf
-- we take the n first elements from the input list and store them in a list
-- we add the list to the final list and iterate through the rest of the list recursively
chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)

-- our input s is a string which represents values on the board
-- we use replacePointsWithZero on s, so all '.' are replaced with '0'
-- we then map digitToInt on every character on s, so we have digits
-- finally, it is zipped with squareStrings to create tuples, e.g ("A1", 0)
parseBoard :: String -> [(String, Int)]
parseBoard s = zip squareStrings (map digToInt $ replacePointsWithZero s)

-- we create three lists of lists and concat them
-- for all the rows, we iterate through the cols and pass the individual numbers to the cross function
-- so if x == 1, we will get "A1", "B1", "C1" etc,
-- similarly for all the columns
-- we use the fact that strings are lists of char, so we can iterate through the string as if its a list
-- and access each char.
-- for the units boxes, we notice that they are in groups of, so we for example iterate through AB, attach the
-- integers to, and repeat the process with CD.
unitList :: [[String]]
unitList =
  [cross rows [x] | x <- cols]
    ++ [cross [x] cols | x <- rows] -- columns
    ++ [cross x y | x <- chunks chunkSize rows, y <- chunks chunkSize cols] -- rows,
    -- boxes

-- using list comprehension, we want to build a list containing elements from unitList,
-- but we have the condition that a sublist must contain the square string s
-- so when the condition is valid. its added to our new list
filterUnitList :: String -> [[String]]
filterUnitList s = [sl | sl <- unitList, s `elem` sl]

-- using list comprehension, we iterate through our squareStrings, but since we want to return
-- the whole filterUnitList, we scope it to a let binding
units :: [(String, [[String]])]
units = [(x, y) | x <- squareStrings, let y = filterUnitList x]

-- using (x:xs) notation, we filter the list so it does't contain the first element x,
-- we pass that filtered list recurisvely to the function, whilst prepending the first element each time
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x : xs) = x : removeDuplicates (filter (/= x) xs)

-- this is our implementation of the delete function, we first check if the head of the list equals our target
-- element. if so, we are done and return the tail of the list. else, we prepend our target element and
-- repeat the same pattern recursively
remove :: Eq a => a -> [a] -> [a]
remove e [] = []
remove e (x : xs)
  | x == e = xs
  | otherwise = x : remove e xs

-- we use map to apply a lambda function to every element in units
-- the lambda function looks at each element, which is a tuple
-- we essentially create a new tuple, where we keep the first element
-- for the second element, which is list of lists, we first flatten it with concat
-- then we remove all duplicate with removeDuplicates
-- after that we delete the squareString in question from our list, and now we have fully transformed it
peers :: [(String, [String])]
peers = map (\(f, s) -> (f, remove f $ removeDuplicates $ concat s)) units

-- using pattern matching with case, we
-- check if the our value, val, is of type Just, then we return the value
-- otherwise, we return the default value a
fromMaybe :: a -> Maybe a -> a
fromMaybe a val =
  case val of
    Just val -> val
    Nothing -> a

-- we lookup the value using our parameter on the peers list
-- then we extract it with fromMaybe, with default param a empty list
getPeers :: String -> [String]
getPeers s = fromMaybe [] $ lookup s peers

-- if its an empty list, we return a empty list
-- we check the head, if its a Nothing object we remove it and continue
-- with the rest of the list, otherwise we prepend the value from the Just object
justifyList :: [Maybe a] -> [a]
justifyList [] = []
justifyList (Nothing : xs) = justifyList xs
justifyList ((Just x) : xs) = x : justifyList xs

-- using list comprehension, we iterate through every element in xs, our input values
-- and calculate the Maybe object, we pass the list to jutsifyList to get only Just values
lookups :: Eq a => [a] -> [(a, b)] -> [b]
lookups xs ys = justifyList [y | x <- xs, let y = lookup x ys]

-- we check if the value is 0, then we return True
-- if the value of the sqaure can be found in the peers, then it's not consistent so we return false
-- otherwise, we return true for the case above
validSquare :: (String, Int) -> [(String, Int)] -> Bool
validSquare t b
  | snd t == 0 = True
  | snd t `elem` lookups (getPeers $ fst t) b = False
  | otherwise = True

-- we first check if the value of the tuple is non-empty, so we return the tuple with
-- the value in a list
-- otherwise we retireve the values for the peers and check if they are in our [1..9] list
validSquareNumbers :: (String, Int) -> [(String, Int)] -> (String, [Int])
validSquareNumbers t b
  | snd t /= 0 = (fst t, [snd t])
  | otherwise = (fst t, reduceList (lookups (getPeers $ fst t) b) [1 .. sudokuSize])

-- we check if every square in a board is a valid square
validBoard :: [(String, Int)] -> Bool
validBoard b = all (`validSquare` b) b

-- applying the validSquareNumbers to every square in the board, producing a list of tuples
-- with the square and it's values
validBoardNumbers :: [(String, Int)] -> [(String, [Int])]
validBoardNumbers b = map (`validSquareNumbers` b) b

-- parse the string into a board and then apply validBoard and validUnits
--verifySudoku :: String -> Bool
verifySudoku :: String -> Bool
verifySudoku s = validUnits (validBoardNumbers board) && validBoard board
  where
    board = parseBoard s

-- using x:xs notation, we take out the head from xs and filter it in our ys list
-- end result will be xs is empty
reduceList :: Eq a => [a] -> [a] -> [a]
reduceList [] [] = []
reduceList [] ys = ys
reduceList (x : xs) ys = reduceList xs (filter (/= x) ys)

-- we iterate through [1..9] and check if it can be inserted in to the values from the units
-- which are produced by looking them up on the board and concatenating them to list
validUnit :: [String] -> [(String, [Int])] -> Bool
validUnit u b = all (\e -> e `elem` concat (lookups u b)) [1 .. sudokuSize]

-- check if all units in a board are valid with validUnit
validUnits :: [(String, [Int])] -> Bool
validUnits b = all (`validUnit` b) unitList

-- function for removing the delimiter string from the files
removeDelimiter :: [String] -> [String]
removeDelimiter = filter (\s -> head s /= '=')

-- function for producing complete sudoku strings from the files
-- input is a list containing all lines from all sudokus, so 9 or 4 lines form a sudoku
-- we take 9(or 4) lines and make a string, doing so recursively for the rest
parseSudokuStrings :: [String] -> [String]
parseSudokuStrings [] = []
parseSudokuStrings xs = concat (take sudokuSize xs) : parseSudokuStrings (drop sudokuSize xs)

-- we read the contents of the file
-- using lines, we get a list of lines, from which we remove the delimiter strings
-- we then map verifySudoku to every sudoku string, created from parseSudokuStrings
main :: IO ()
main = do
  putStrLn "Enter file name:"
  fileName <- getLine
  contents <- readFile fileName
  let board = removeDelimiter $ lines contents
  mapM_ (print . verifySudoku) (parseSudokuStrings board)