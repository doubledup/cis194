-- CIS 194 Homework 3

module Golf where

--1-----------------------------------------------------------------------------
skips :: [a] -> [[a]]
skips l = map (\n -> map fst . filter (\x -> mod (snd x) n == 0) $ zip l s) s
    where s = [1..length l]

--2-----------------------------------------------------------------------------
t :: [a] -> [(a, a, a)]
t (a:b:c:xs) = (a,b,c):(t $ b:c:xs)
t _ = []

localMaxima :: [Integer] -> [Integer]
localMaxima l = map (\(x, y, z) -> y) . filter (\(x, y, z) -> x < y && y > z) $ t l

--3-----------------------------------------------------------------------------
-- p stands for 'partial histogram'. Given a list of Ints representing the
-- counts of numbers from 0 upwards, create a string representing a histogram of
-- these values without any axes.
p :: [Int] -> String
p l
  | all (==0) l = ""
  | otherwise = [if x == maximum l then '*' else ' ' | x <- l]
                ++ "\n"
                ++ p [if x == maximum l then x - 1 else x | x <- l]

histogram :: [Integer] -> String
histogram l = p (zipWith (\ x y -> length . filter (==x) $ y) [0..9] (repeat l))
              ++ "\n==========\n0123456789\n"


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
