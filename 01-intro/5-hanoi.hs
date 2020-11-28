type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = (hanoi (n-1) a c b) ++ [(a, b)] ++ (hanoi (n-1) c b a)

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 a b _ _ = []
hanoi4 1 a b _ _ = [(a, b)]
hanoi4 2 a b c _ = [(a, c), (a, b), (c, b)]
hanoi4 n a b c d
  | n > 0 = let

      disksToMove = n - 1
      -- not sure about the optimal ratio here for larger numbers of disks
      topHalfSize = 0.5
      topHalf = ceiling ((fromIntegral disksToMove) * topHalfSize)
      bottomHalf = disksToMove - topHalf

    in
      (hanoi4 topHalf a c b d) ++ (hanoi bottomHalf a d b) ++
      [(a, b)] ++
      (hanoi bottomHalf d b a) ++ (hanoi4 topHalf c b a d)
  | otherwise = []

printHanoi :: (Integer -> [Move]) -> [Integer] -> IO [[()]]
printHanoi f nums =
  nums
    |> map (\n -> let
        hanoi = f n
        len = length hanoi
      in
        [
          print $ (show n) ++ " disks, " ++ (show len) ++ " moves",
          print hanoi
        ]
        |> sequence)
    |> sequence

(|>) = flip ($)

main = let
    nums = [1..10]
  in do
    print $ "3-peg hanoi:"
    printHanoi (\n -> hanoi n "a" "b" "c") nums
    print $ "4-peg hanoi:"
    printHanoi (\n -> hanoi4 n "a" "b" "c" "d") nums
