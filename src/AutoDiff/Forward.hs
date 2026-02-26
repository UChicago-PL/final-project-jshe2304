-- | Forward-mode automatic differentiation.
module AutoDiff.Forward
    ( var
    , constant
    , diff
    , partialDiff
    , grad
    ) where

import AutoDiff.Types

-- | Create a variable for differentiation.
var :: Num a => a -> Dual a
var x = Dual x 1

-- | Create a constant (derivative is zero).
constant :: Num a => a -> Dual a
constant x = Dual x 0

-- | Differentiation combinator returning the derivative as a first-class function. 
diff :: Num a => (Dual a -> Dual a) -> (a -> a)
diff f = \x -> tangent (f (var x))

-- | Compute partial derivative i of a multivariable function f, on input xs
partialDiff :: Num a => Int -> ([Dual a] -> Dual a) -> [a] -> a
partialDiff i f xs = diff fi (xs !! i)
  where
    fi xi = f [if i == j then xi else constant xj | (j, xj) <- zip [0..] xs]

-- | Compute the gradient of a function over a list of inputs.
grad :: Num a => ([Dual a] -> Dual a) -> [a] -> [a]
grad f xs = [partialDiff i f xs | i <- [0 .. length xs - 1]]
