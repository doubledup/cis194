module Golf where

main :: IO ()
main = do
  print $ skips "ABCD"
  print $ skips "hello!"
  print $ skips ([] :: [Bool])
  print $ skips [1]
  print $ skips [True, False]

skips :: [a] -> [[a]]
skips x =
  map (t) $ zip [1..length x] (repeat x)

t :: (Int, [a]) -> [a]
t (n, x)
  = map snd
  . filter (\(i, _) -> i `mod` n == 0)
  $ zip [1..length x] x
