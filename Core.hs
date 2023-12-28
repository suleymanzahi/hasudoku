module Core where

--------Core functions-----------------
cross :: [a] -> [a] -> [[a]]
cross s1 s2 = [[r, c] | r <- s1, c <- s2]

rowBoxes, colBoxes :: [String]
rowBoxes = ["ABC", "DEF", "GHI"]
colBoxes = ["123", "456", "789"]

rows, cols :: String
rows = concat rowBoxes
cols = concat colBoxes

squares :: [String]
squares = cross rows cols

unitlist :: [[String]]
unitlist =
  [cross rows [c] | c <- cols]
    ++ [cross [r] cols | r <- rows]
    ++ [cross rs cs | rs <- rowBoxes, cs <- colBoxes]

units :: [(String, [[String]])]
units = [(s, filter (elem s) unitlist) | s <- squares]

type Board = [(String, [Int])]

allDigits :: [Int]
allDigits = [1, 2, 3, 4, 5, 6, 7, 8, 9]

infAllDigits = repeat allDigits

emptyBoard = zip squares infAllDigits

parseSquare :: (String, Char) -> Board -> Maybe Board
parseSquare (s, x) values
  | x == '.' || x == '0' = return values
  | isDig x = assign (digToInt x) s values
  | otherwise = fail "not a valid grid"

parseBoard :: String -> Maybe Board
parseBoard = foldr ((=<<) . parseSquare) (Just emptyBoard) . zip squares

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just x) = x
fromMaybe y Nothing = y

-- using (x:xs) notation, we filter the list so it does't contain the first element x,
-- we pass that filtered list recurisvely to the function, whilst prepending the first element each time
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x : xs) = x : removeDuplicates (filter (/= x) xs)

-- we use map to apply a lambda function to every element in units
-- the lambda function looks at each element, which is a tuple
-- we essentially create a new tuple, where we keep the first element
-- for the second element, which is list of lists, we first flatten it with concat
-- then we remove all duplicate with removeDuplicates
-- after that we delete the squareString in question from our list, and now we have fully transformed it
peers :: [(String, [String])]
peers = map (\(f, s) -> (f, remove f $ removeDuplicates $ concat s)) units

-- we convert each char to a string, then we use
-- the read function to convert it to a Int
digToInt :: Char -> Int
digToInt c = read [c] :: Int

-- function for replacing isDigit
-- if a character exists in the array 0-9, then it's a Char
isDig :: Char -> Bool
isDig c
  | c `elem` ['0' .. '9'] = True
  | otherwise = False


--our implementation of chunksOf
-- we take the n first elements from the input list and store them in a list
-- we add the list to the final list and iterate through the rest of the list recursively
chunks :: Int -> String -> [String]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)

-- type signature gives us a pair of functions, a pair as arguments and a pair as return value
-- return type of the pairs match return type of functions, so we apply the respective function
-- to each element in the pair
map2 :: (a -> c, b -> d) -> (a, b) -> (c, d)
map2 (f, g) (x, y) = (f x, g y)

-- we have a mapping function and a boolean function,
-- if the boolean function applied on the element is true, we will map the our function, otherwise
-- leave the element as is
mapIf :: (a -> a) -> (a -> Bool) -> [a] -> [a]
mapIf f b = map (\e -> if b e then f e else e)

-- using pattern matching, if the first element is a Just, return it
-- if it's a Nothing, remove it keep searching the list
firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust ((Just x) : xs) = Just x
firstJust (Nothing : xs) = firstJust xs

-- function for cheking if a Maybe contains a Nothing element
isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

-- if any of the elements are a just, return them
-- if both are Nothing, return Nothing
maybeOr :: Maybe a -> Maybe a -> Maybe a
maybeOr (Just x) _ = Just x
maybeOr _ (Just x) = Just x
maybeOr Nothing Nothing = Nothing

-- if the first element of the tuple equals our target element, return the second element
-- of the tuple, otherwise keep searching rest of the list
lookupList :: Eq a => a -> [(a, [b])] -> [b]
lookupList _ [] = []
lookupList e ((x, y) : xs) = if e == x then y else lookupList e xs

-- if the element is a Nothing, return Nothing
-- if it's a Just, extract the underlying element, and apply f on it
maybeBind :: Maybe a -> (a -> Maybe b) -> Maybe b
maybeBind Nothing f = Nothing
maybeBind (Just x) f = f x

-- this is our impl of the delete function, we first check if the head of the list equals our target
-- element. if so, we are done and return the tail of the list. else, we prepend our target element and
-- repeat the same pattern recursively
remove :: Eq a => a -> [a] -> [a]
remove e [] = []
remove e (x : xs)
  | x == e = xs
  | otherwise = x : remove e xs

-- using mapIf, we search for the tuple we are interested in
-- using map2, we first apply id to square, since it should remain the same
-- on the list we apply const/remove respectively to either set or remove a value
eliminateValue, setValue :: Int -> String -> Board -> Board
setValue v s = mapIf (map2 (id, const [v])) (\(x, y) -> s == x)
eliminateValue v s = mapIf (map2 (id, remove v)) (\(x, y) -> s == x)

-- after elimination of a value, if the list is empty, there are no possible values to insert
-- so the board is invalid. otherwise return the new board
eliminate :: Int -> String -> Board -> Maybe Board
eliminate v s b
  | null newList = Nothing
  | otherwise = Just newBoard
  where
    newBoard = eliminateValue v s b
    newList = lookupList s newBoard

-- compute the eliminate function, which will return a Maybe Board
-- the maybeBoard can be Nothing or Just, we want to pass it to this function recursively
-- the recursion will handle the rest of the squares
assign' :: Int -> [String] -> Board -> Maybe Board
assign' _ [] b = Just b
assign' v (x : xs) b = maybeBind (eliminate v x b) (assign' v xs)

-- calls the assign' function
assign :: Int -> String -> Board -> Maybe Board
assign v s b = assign' v (lookupList s peers) (setValue v s b)

-- we pass the value of assign to solveSudoku,
--solveSudoku will extract the the board via the bind function and apply its function
-- if it returns a Just board, we will continues with the recursion
-- otherwise, if its Nothing, it will try another value from lookupList x b
solveSudoku' :: [String] -> Board -> Maybe Board
solveSudoku' (x : xs) b =
  firstJust (map (\v -> assign v x b `maybeBind` solveSudoku' xs) (lookupList x b))
solveSudoku' [] b = Just b

-- we parse the board from the input string and pass it to solveSudoku via bind
solveSudoku :: String -> Maybe Board
solveSudoku s = parseBoard s `maybeBind` solveSudoku' squares