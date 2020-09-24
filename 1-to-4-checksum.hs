import Data.Char (digitToInt)

toDigits :: Integer -> [Integer]
toDigits i = charsToIntegers . show $ i

charsToIntegers :: [Char] -> [Integer]
charsToIntegers = (map $ toInteger . digitToInt)

-- main = print . toDigits $ 421

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherFromLeft . reverse

doubleEveryOtherFromLeft :: [Integer] -> [Integer]
doubleEveryOtherFromLeft [] = []
doubleEveryOtherFromLeft (x:[]) = [x]
doubleEveryOtherFromLeft (x:y:xs) = x : (2*y) : doubleEveryOtherFromLeft xs

-- main = print $ doubleEveryOther [1, 1, 2, 3, 5, 8, 13]

sumDigits :: [Integer] -> Integer
sumDigits = sum . (map $ sum . toDigits)

-- main = print $ sumDigits [1, 131, 24, 42, 987]

validate :: Integer -> Bool
validate = (\x -> x `mod` 10 == 0) . sumDigits . doubleEveryOther . toDigits

main = do
  print $ validate 4012888888881881
  print $ validate 4012888888881882
