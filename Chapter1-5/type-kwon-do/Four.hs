munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge xToY yToWZ x = fst $ yToWZ $ xToY x
