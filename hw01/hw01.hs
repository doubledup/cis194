import Data.Char (digitToInt)

-- CIS 194 Homework 1

-- Exercise 1

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0    = []
  | otherwise = map (toInteger . digitToInt) $ show n

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse . toDigits $ n

-- Exercise 2

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther l = reverse $ mapEveryOther (*2) (reverse l)

mapEveryOther :: (a -> a) -> [a] -> [a]
mapEveryOther f l = map (\(i, x) -> if i `mod` 2 == 0 then f x else x) $ zip [1..] l

-- Exercise 3

sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits)

--Exercise 4

validate :: Integer -> Bool
validate n = mod (sumDigits . doubleEveryOther . toDigits $ n) 10 == 0

-- Exercise 5

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
  | otherwise = (hanoi4 topHalf a c b d) ++
                (hanoi bottomHalf a d b) ++
                [(a, b)] ++
                (hanoi bottomHalf d b a) ++
                (hanoi4 topHalf c b a d)
  where topHalf = floor (((fromInteger n) - 1) / 2)
        bottomHalf = ceiling (((fromInteger n) - 1) / 2)

main = do
  print . toDigits $ 4012888888881881
  print . doubleEveryOther . toDigits $ 4012888888881881
  print . sumDigits . doubleEveryOther . toDigits $ 4012888888881881
  print . validate $ 4012888888881881
  print . validate $ 4012888888881882
  print (hanoi 15 "a" "b" "c")

