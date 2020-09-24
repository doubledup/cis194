import Data.Char (digitToInt)

toDigits :: Integer -> [Integer]
toDigits i = charsToIntegers . show $ i

charsToIntegers :: [Char] -> [Integer]
charsToIntegers = (map $ toInteger . digitToInt)

main = print . toDigits $ 421
