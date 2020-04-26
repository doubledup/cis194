-- CIS 194 Homework 3

module Golf where

--1-----------------------------------------------------------------------------
skips :: [a] -> [[a]]
skips l = map (\n -> map fst . filter (\x -> mod (snd x) n == 0) $ zip l s) s
    where s = [1..length l]

--2-----------------------------------------------------------------------------
localMaxima :: [Integer] -> [Integer]
localMaxima (b:c:[]) = []
localMaxima (a:b:c:x)
  | and [a < b, b < c] = b:(localMaxima $ b:c:x)
  | otherwise          = localMaxima $ b:c:x


--3-----------------------------------------------------------------------------
-- Given a list l and a number n, count the occurrences of n in l
c :: [Integer] -> Integer -> Integer
c [] _ = 0
c l n = toInteger . length $ filter (\x -> x == n) l

-- Given a list l and a pair of inclusive bounds (low, hi), return the number
-- of times each number in the bounds appears in l.
f :: [Integer] -> (Integer, Integer) -> [Integer]
f l (low, hi) = map (c l) [low..hi]

-- Given a list l of the count of numbers (where each number is given by the
-- relevant index), construct a histogram of these numbers without labels for
-- axes.
g :: [Integer] -> String
g [] = ""
g l
  | length l == length (filter (\x -> x == 0) l) = ""
  | otherwise = g (map (\x -> if x > 0 then x - 1 else x) l)
                ++ "\n"
                ++ (map (\x -> if x > 0 then '*' else ' ') l)

-- Given a list l of numbers, create a histogram with a labelled horizontal
-- axis.
histogram :: [Integer] -> String
histogram l = g (f l (0,9)) ++ "\n==========\n0123456789\n"

main :: IO()
main = do
  --print "test cases for skips:"
  --print $ skips "ABCD"
  --print $ skips "hello!"
  --print $ skips [1]
  --print $ skips [True, False]
  --print $ skips ([] :: [Int])

  --print $ skips "hello!"
  --print $ skips ""
  --print $ skips [1..10]

  --print "test cases for localMaxima"
  --print $ localMaxima [2,9,5,6,1]
  --print $ localMaxima [2,3,4,1,5]
  --print $ localMaxima [1..5]
  --print $ localMaxima [9,3,5,4,7,7,7,45,3]

  print $ c [3,6,5,7,4,1,3,3,6,5,2,8,9,0,7,3] 3

  print $ histogram [1,1,1,5]
