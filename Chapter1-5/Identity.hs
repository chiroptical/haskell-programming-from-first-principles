newtype Identity a = Identity a

instance Eq a => Eq (Identity a) where
  (==) (Identity x) (Identity x') = x == x'
