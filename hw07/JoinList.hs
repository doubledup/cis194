module JoinList where

import Sized

-- ex 1

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
jl1 +++ jl2 = 
  let
    m1 = getTag jl1
    m2 = getTag jl2
  in
    Append (mappend m1 m2) jl1 jl2

getTag :: Monoid m => JoinList m a -> m
getTag Empty          = mempty
getTag (Single m a)   = m
getTag (Append m _ _) = m

-- ex 2

indexJ :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i (Single m a)
  | i == 0    = Just a
  | otherwise = Nothing
indexJ i (Append m jl1 jl2)
  | i < 0 || (size m) <= (Size i) = Nothing
  | otherwise =
    let 
      m1  = getTag jl1
      sm1 = size m1
      si  = Size i
    in
      if si < sm1 then
        indexJ i jl1
      else
        indexJ (getSize (si - sm1)) jl2
