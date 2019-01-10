# chapter

(.) :: (b -> c) -> (a -> b) -> (a -> c)
fmapF :: Functor f => (m -> n) -> f m -> f n
fmapG :: Functor g => (x -> y) -> g x -> g y

(fmapF . fmapG)

1. z = (.) fmapF
    LeftOperand (b -> c): (m -> n) -> (f m -> f n)
    RightOperand (a -> b): a -> (m -> n)
    Result (a -> c): a -> (f m -> f n)

2. z fmapG
    (m -> n) == (g x -> g y)
    LeftOperand (b -> c): (g x -> g y) -> (f (g x) -> f (g y))
    RightOperand (a -> b): (x -> y) -> (g x -> g y)
    Result (a -> c): (x -> y) -> (f (g x) -> f (g y))
    
