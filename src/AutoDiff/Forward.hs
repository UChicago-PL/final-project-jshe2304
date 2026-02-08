-- | Forward-mode automatic differentiation.
module AutoDiff.Forward
    ( var
    , constant
    , diff
    ) where

import AutoDiff.Types

-- | Create a variable for differentiation.
var :: Num a => a -> Dual a
var x = Dual x 1

-- | Create a constant (derivative is zero).
constant :: Num a => a -> Dual a
constant x = Dual x 0

-- | Differentiate a function at a point.
diff :: Num a => (Dual a -> Dual a) -> a -> a
diff f x = tangent (f (var x))
