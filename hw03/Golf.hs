-- CIS 194 Homework 3

module Golf where

--1-----------------------------------------------------------------------------
-- e stands for 'every n'. It takes every nth item from a list l.
-- Each item in l is paired with its position (a number). Then this list of
-- pairs is filtered so that only those whose position is divisible by n remain.
-- Finally, the items are extracted from the pairs.
e :: [a] -> Int -> [a]
e l n = map fst $ filter (\x -> mod (snd x) n == 0) $ zip l [1..length l]

skips :: [a] -> [[a]]
skips l = map (e l) [1..length l]


--2-----------------------------------------------------------------------------
localMaxima :: [Integer] -> [Integer]
localMaxima (b:c:[]) = []
localMaxima (a:b:c:x)
  | and [a < b, b < c] = b:(localMaxima $ b:c:x)
  | otherwise          = localMaxima $ b:c:x


--3-----------------------------------------------------------------------------
-- Given a list l and a number n, count the occurrences of n in l
countn :: [Integer] -> Integer -> Integer
countn [] _ = 0
countn l n = toInteger . length $ filter (\x -> x == n) l

-- Given a list l and a pair of inclusive bounds (low, hi), return the number
-- of times each number in the bounds appears in l.
freq :: [Integer] -> (Integer, Integer) -> [Integer]
freq l (low, hi) = map (countn l) [low..hi]

graph :: [Integer] -> String
graph [] = ""
graph l
  | length l == length (filter (\x -> x == 0) l) = ""
  | otherwise = graph (map (\x -> if x > 0 then x - 1 else x) l)
                ++ "\n"
                ++ (map (\x -> if x > 0 then '*' else ' ') l)

histogram :: [Integer] -> String
histogram l = graph (freq l (0,9)) ++ "\n==========\n0123456789\n"

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

  print $ countn [3,6,5,7,4,1,3,3,6,5,2,8,9,0,7,3] 3

  print $ histogram [1,1,1,5]
