import Data.Char

-- 1

-- a
sumSquaresA :: (Eq p, Num p) => p -> p
sumSquaresA 0 = 0
sumSquaresA x = x * 2 + sumSquaresA (x -1)

-- b
sumSquaresB :: (Num a, Enum a) => a -> a
sumSquaresB x = sum [x * 2 | x <- [1 .. x]]

-- c
sumSquaresC :: (Num a, Enum a) => a -> a
sumSquaresC = \x -> sum (map (* 2) [1 .. x])

-- 2
findOccurrences :: (Eq t, Num a) => [t] -> t -> [a]
findOccurrences s c = findfindOccurrencesRec s c 0

findfindOccurrencesRec :: (Eq t, Num a) => [t] -> t -> a -> [a]
findfindOccurrencesRec [] _ _ = []
findfindOccurrencesRec (s : xs) c i
  | s == c = i : findfindOccurrencesRec xs c (i + 1)
  | otherwise = findfindOccurrencesRec xs c (i + 1)

-- 3
-- map
myMap :: (t -> a) -> [t] -> [a]
myMap f l = [f x | x <- l]

-- 4
data Expr = T | F | And Expr Expr | Or Expr Expr | Not Expr deriving (Show)

eval :: Expr -> Expr
eval T = T
eval F = F
eval (Not e) = case eval e of
  T -> F
  F -> T
eval (Or l r) = case eval l of
  T -> T
  F -> case eval r of
    T -> T
    F -> F
eval (And l r) = case eval l of
  F -> F
  T -> case eval r of
    T -> T
    F -> F

-- 5
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x : y : l) = x <= y && isSorted (y : l)

-- 6
find :: Eq a1 => a1 -> [(a1, a2)] -> Maybe a2
find _ [] = Nothing
find targetKey ((k, v) : dict)
  | targetKey == k = Just v
  | otherwise = find targetKey dict

-- 7
findOrDefault :: Eq t1 => t2 -> t1 -> [(t1, t2)] -> t2
findOrDefault defaultValue _ [] = defaultValue
findOrDefault defaultValue targetKey ((k, v) : dict)
  | targetKey == k = v
  | otherwise = findOrDefault defaultValue targetKey dict

-- 8
findOrError :: Eq t => t -> [(t, p)] -> p
findOrError _ [] = error "No such key"
findOrError targetKey ((k, v) : dict)
  | targetKey == k = v
  | otherwise = findOrError targetKey dict

-- 9
replace :: [(Char, Char)] -> [Char] -> [Char]
replace dict s = [findOrDefault c c dict | c <- s]

-- 10
data Account = Administrator String | User String [String] | Guest

-- 11
isAllowedAccess :: Account -> Bool
isAllowedAccess (Administrator _) = True
isAllowedAccess (User _ p) = "write" `elem` p

-- 12
data StringExpr
  = Constant String
  | Reverse StringExpr
  | Concatenation StringExpr StringExpr
  | Remove StringExpr StringExpr
  | Zip StringExpr StringExpr
  deriving (Show)

-- 13
evalStringExpr :: StringExpr -> String
evalStringExpr (Constant se) = se
evalStringExpr (Reverse se) = reverse (evalStringExpr se)
evalStringExpr (Concatenation s1 s2) = evalStringExpr s1 ++ evalStringExpr s2
evalStringExpr (Remove s1 s2) = [x | x <- evalStringExpr s1, x `elem` evalStringExpr s2]
evalStringExpr (Zip s1 s2) = concat [x : [y] | (x, y) <- zip (evalStringExpr s1) (evalStringExpr s2)]

-- 14
-- 5 :: Num a => a
-- 2 ∗ 4 :: Num a => a
-- 1.5 + 1 :: Fractional a => a
-- (2 +) :: Num a => a -> a
-- (+ 2) :: Num a => a -> a
-- (+) 2 :: Num a => a -> a
-- (\x -> x + 2) :: Num a => a -> a
-- (\( x, y) -> x && y) :: (Bool, Bool) -> Bool
-- (\( x, y, z) -> z) :: (a, b, c) -> c
-- (\(_, y, _) -> y) :: (a, b, c) -> b

-- 15
square :: Int -> Int
square = \x -> x * x

swap :: (a, b) -> (b, a)
swap = \(x, y) -> (y, x)

greet :: String -> String
greet = \name -> "Hello, " ++ name ++ "!"

-- 16
removeEquals :: [(Int, Int)] -> [(Int, Int)]
removeEquals = filter (uncurry (/=))

-- 17
foo :: Integer -> Integer -> Integer
foo x y = bar (x, y)

bar :: Num a => (a, a) -> a
bar (x, y) = x + y

-- 18
one :: [[Char]]
one = ["a", "b", "c"]

two :: [Char]
two = ['a', 'b', 'c']

three :: [[a] -> [a]]
three = [tail, init, reverse]

four :: [([Char], Integer)]
four = [("a", 0), ("b", 1)]

-- 19
func1 :: a -> b -> a
func1 x y = x

func2 :: (a, b) -> a
func2 (x, y) = x

func3 :: (a, b) -> b
func3 (x, y) = y

func4 :: a -> b -> c -> ((a, b), c)
func4 x y z = ((x, y), z)

-- 20
isAlphabetic :: String -> Bool
isAlphabetic s = filter isAlpha s == s

-- 21
evens :: [Int] -> [Int]
evens = filter even