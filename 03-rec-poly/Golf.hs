module Golf where

main :: IO ()
main = do
  print $ skips "ABCD"
  print $ skips "hello!"
  print $ skips ([] :: [Bool])
  print $ skips [1]
  print $ skips [True, False]

skips xs =
  map (takeEveryNth) $ zip [1..length xs] (repeat xs)

takeEveryNth (n, xs)
  = map (\(_b, x) -> x)
  . filter (\(b, _x) -> b)
  . map (\(i, x) -> if i `mod` n == 0 then (True, x) else (False, x))
  $ zip [1..length xs] xs
