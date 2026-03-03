module AutoDiff.Forward(var, constant, diff, partialDiff, grad) where

import AutoDiff.Types

-- | A differentiable variable
var :: Num a => a -> Dual a
var x = Dual x 1

-- | A constant (derivative is zero)
constant :: Num a => a -> Dual a
constant x = Dual x 0

-- | Returns the derivative of f
diff :: Num a => (Dual a -> Dual a) -> (a -> a)
diff f = \x -> tangent (f (var x))

-- | Returns the i-th partial derivative of f
partialDiff :: Num a => Int -> ([Dual a] -> Dual a) -> ([a] -> a)
partialDiff i f = \xs ->
    let fi xi = f [if i == j then xi else constant xj | (j, xj) <- zip [0..] xs]
    in diff fi (xs !! i)

-- | Returns the gradient of a function at an input
grad :: Num a => ([Dual a] -> Dual a) -> [a] -> [a]
grad f xs = [partialDiff i f xs | i <- [0 .. length xs - 1]]
