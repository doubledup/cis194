main :: IO ()
main = let
  exampleIntList = (ConsIntList 4
    (ConsIntList 5
    (ConsIntList 6
    (ConsIntList 7
    (ConsIntList 1
      EmptyIntList)))))
  examplePolyList = (ConsPolyList 4
    (ConsPolyList 5
    (ConsPolyList 6
    (ConsPolyList 7
    (ConsPolyList 1
      EmptyPolyList)))))
  in
  do
    print "hi"
    print exampleIntList
    print $ mapIntList (+2) exampleIntList
    print $ filterIntList even exampleIntList
    print . reverse $ foldlIntList (flip (:)) [] exampleIntList
    print examplePolyList
    print $ filterPolyList even examplePolyList

data IntList
  = EmptyIntList
  | ConsIntList Int IntList
  deriving Show

mapIntList :: (Int -> Int) -> IntList -> IntList
mapIntList _ EmptyIntList = EmptyIntList
mapIntList f (ConsIntList i lst) = ConsIntList (f i) (mapIntList f lst)

filterIntList :: (Int -> Bool) -> IntList -> IntList
filterIntList _ EmptyIntList = EmptyIntList
filterIntList f (ConsIntList i lst)
  | f i == True = ConsIntList i (filterIntList f lst)
  | otherwise = filterIntList f lst

foldlIntList :: (a -> Int -> a) -> a -> IntList -> a
foldlIntList _ acc EmptyIntList = acc
foldlIntList f acc (ConsIntList i lst) = foldlIntList f (f acc i) lst

data PolyList t
  = EmptyPolyList
  | ConsPolyList t (PolyList t)
  deriving Show

filterPolyList _ EmptyPolyList = EmptyPolyList
filterPolyList f (ConsPolyList val lst)
  | f val = ConsPolyList val rest
  | otherwise = rest
  where
    rest = filterPolyList f lst
