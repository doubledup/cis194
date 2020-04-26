module Fibonacci where
import Data.List (foldl')

-- ex 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n
  | n < 0     = fp2 - fp1
  | otherwise = fm2 + fm1
  where
    fp1 = fib (n+1)
    fp2 = fib (n+2)
    fm1 = fib (n-1)
    fm2 = fib (n-2)

fibs :: [ Integer ]
fibs = map fib [0..]

-- ex 2
fibs2 :: [Integer]
fibs2 = [0]

-- ex 3
data Stream a = Item a ( Stream a )

streamToList :: Stream a -> [ a ]
streamToList (Item a s) = a : ( streamToList s )

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

-- ex 4
constructRepeater :: (a -> b -> b) -> a -> b
constructRepeater f x = f x $ constructRepeater f x

streamRepeat :: a -> Stream a
streamRepeat = constructRepeater Item

-- streamMod7Repeat = constructRepeater (\x s -> Item (x `mod` 7) s)

-- listToStream :: [a] -> Stream a
-- listToStream (x:xs) = Item x $ listToStream xs

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Item x s) = Item ( f x ) ( streamMap f s )

-- collatz :: Integer -> Integer
-- collatz 1 = 1
-- collatz x
--   | x `mod` 2 == 0 = x `div` 2
--   | otherwise      = 3*x+1

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Item ( newval ) ( streamFromSeed f newval )
  where newval = f a

-- ex 5
nats :: Stream Integer
nats = streamFromSeed (+1) (-1)

interleaveStreams' :: Stream a -> Stream a -> Stream a
interleaveStreams' (Item x1 s1) (Item x2 s2) = Item x1 (Item x2 (interleaveStreams s1 s2))

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Item x s1) s2 = Item x (interleaveStreams' s2 s1)

foldrStreams :: (a -> b -> b) -> b -> Stream a -> b
foldrStreams f a (Item x ss) = f x ( foldrStreams f a ss )

-- interleave :: [a] -> [a] -> [a]
-- interleave (x:xs) (y:ys) = x : y : (interleave xs ys)
-- interleave xs ys = xs ++ ys

-- interleaving :: [Integer]
-- interleaving = foldr interleave [] ( map repeat [0..100] )

ruler :: Stream Integer
ruler = foldrStreams interleaveStreams undefined ( streamMap streamRepeat nats )
