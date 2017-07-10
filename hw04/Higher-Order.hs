-- ex1
fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) .
        iterate (\x -> if even x then div x 2 else 3 * x + 1)

-- ex2
type Height = Integer

data Tree a = Leaf
            | Node Height (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr balancedAdd Leaf

getHeight :: Tree a -> Height
getHeight Leaf           = 0
getHeight (Node h _ _ _) = h

-- calculate the height of a node in a binary tree given its two children
newHeight :: Tree a -> Tree a -> Height
newHeight left right = succ . foldr max 0 $ map getHeight [left, right]

balancedAdd :: a -> Tree a -> Tree a
balancedAdd x Leaf = Node 0 Leaf x Leaf
balancedAdd x (Node 0 Leaf y Leaf) = Node 1 (Node 0 Leaf x Leaf) y Leaf
balancedAdd x (Node 1 left_child y Leaf) = Node 1 left_child y (Node 0 Leaf x Leaf)
balancedAdd x (Node _ left@(Node height_left _ _ _) y right@(Node height_right _ _ _))
    | height_left < height_right = let new_left = balancedAdd x left
        in Node (newHeight new_left right) new_left y right
    | otherwise = let new_right = balancedAdd x right
        in Node (newHeight left new_right) left y new_right

-- ex3
xor :: [Bool] -> Bool
xor = foldr (\x y -> (x && not y) || (y && not x)) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> f x : y) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f = foldr (flip f)

-- ex4
genList :: Integer -> [Integer]
genList n = let exclude_these = [x+y+2*x*y | x <- [1..(quot n 3)], y <- [1..(quot n 3)], x <= y, x+y+2*x*y <= n]
            in [x | x <- [1..n], x `notElem` exclude_these]

sieveSundaram :: Integer -> [Integer]
sieveSundaram = map (\x -> 2*x+1) . genList
