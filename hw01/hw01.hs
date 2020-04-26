-- CIS 194 Homework 1

-- Exercise 1

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0    = []
  | otherwise = map (toInteger . digitToInt) $ show n

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse . toDigits $ n

timesN :: Integer -> Integer -> Integer
timesN n = (* n)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther l = reverse $ mapEveryOther (timesN 2) (reverse l)

mapEveryOther :: (a -> a) -> [a] -> [a]
mapEveryOther f (x:y:l)
  | length l `mod` 2 == 0 = x : (f y) : (mapEveryOther f l)
  | otherwise             = (f x) : y : (mapEveryOther f l)
mapEveryOther f (x:[]) = [x]
mapEveryOther f [] = []

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = (sumDigits xs) + (sum . toDigits $ x)

validate :: Integer -> Bool
validate n = mod (sumDigits . doubleEveryOther . toDigits $ n) 10 == 0

type Peg = String
type Move = (String, String)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n < 1  = []
  | n == 1 = [(a, b)]
  | otherwise = (hanoi (n - 1) a c b) ++ [(a, b)] ++ (hanoi (n - 1) c b a)

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n a b c d
  | n < 1  = []
  | n == 1 = [(a, b)]
  | n == 2 = [(a, c)]

main = do
  print . toDigits $ 4012888888881881
  print . doubleEveryOther . toDigits $ 4012888888881881
  print . sumDigits . doubleEveryOther . toDigits $ 4012888888881881
  print . validate $ 4012888888881881
  print . validate $ 4012888888881882
  print (hanoi 15 "a" "b" "c")