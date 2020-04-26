module JoinList where

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
getTag Empty = mempty
getTag (Single m a) = m
getTag (Append m _ _) = m
