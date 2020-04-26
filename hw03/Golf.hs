-- CIS 194 Homework 3

module Golf where

-- e stands for 'every n'. It takes every nth item from a list l.
-- Each item in l is paired with its position (a number). Then this list of
-- pairs is filtered so that only those whose position is divisible by n remain.
-- Finally, the items are extracted from the pairs.
e :: [a] -> Int -> [a]
e l n = map fst $ filter (\x -> mod (snd x) n == 0) $ zip l [1..length l]

skips :: [a] -> [[a]]
skips l = map (e l) [1..length l]

localMaxima :: [Integer] -> [Integer]
localMaxima (b:c:[]) = []
localMaxima (a:b:c:x)
  | and [a < b, b < c] = b:(localMaxima $ b:c:x)
  | otherwise          = localMaxima $ b:c:x

main :: IO()
main = do
  print "test cases for skips:"
  print $ skips "ABCD"
  print $ skips "hello!"
  print $ skips [1]
  print $ skips [True, False]
  print $ skips ([] :: [Int])

  print $ skips "hello!"
  print $ skips ""
  print $ skips [1..10]

  print "test cases for localMaxima"
  print $ localMaxima [2,9,5,6,1]
  print $ localMaxima [2,3,4,1,5]
  print $ localMaxima [1..5]

  print $ localMaxima [9,3,5,4,7,7,7,45,3]
