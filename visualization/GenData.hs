module Main where

import AutoDiff

-- Sample a function and its first three derivatives over a range, writing a CSV file.
writeSample :: String -> (Double, Double) -> Int -> (forall a. Floating a => a -> a) -> IO ()
writeSample name (lo, hi) n f = do
    let path = "visualization/data/derivatives/" ++ name ++ ".csv"
        header = "x,f(x),f'(x),f''(x),f'''(x)"
        -- Create derivative functions
        d1 = diff f
        d2 = (diff . diff) f
        d3 = (diff . diff . diff) f
        -- Create domain by interpolating from lo to hi
        domain = [lo + (hi - lo) * fromIntegral i / fromIntegral (n - 1) | i <- [0 .. n - 1]]
        -- Helper to create CSV row of derivative values
        createLine x = show x ++ "," ++ show (f x) ++ "," ++ show (d1 x) ++ "," ++ show (d2 x) ++ "," ++ show (d3 x)
        rows = [createLine x | x <- domain]
    writeFile path (unlines (header : rows))

main :: IO ()
main = do
    writeSample "polynomial" (-2.5, 2.5) 200 (\x -> x*x*x*x - 3*x*x + 2)
    writeSample "sinusoidal" (-2*pi, 2*pi) 200 sin
    writeSample "exponential" (-2, 3) 200 exp
    writeSample "composite" (-3, 3) 200 (\x -> exp (negate (x*x)))
