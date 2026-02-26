-- | Generate CSV data for a 2D gradient field visualization.
module Main where

import AutoDiff

-- | The function to visualize: f(x, y) = sin(x) * cos(y)
f :: Floating a => [a] -> a
f [x, y] = sin x * cos y
f _ = error "f expects exactly 2 inputs"

main :: IO ()
main = do
    let path = "visualization/data/gradient_field.csv"
        n = 50
        lo = -pi
        hi = pi
        step = (hi - lo) / fromIntegral (n - 1)
        header = "x,y,f,df_dx,df_dy"
        rows =
            [ show x ++ "," ++ show y ++ "," ++ show (f [x, y]) ++ "," ++ show gx ++ "," ++ show gy
            | i <- [0 .. n - 1]
            , let x = lo + step * fromIntegral i
            , j <- [0 .. n - 1]
            , let y = lo + step * fromIntegral j
            , let [gx, gy] = grad f [x, y]
            ]
    writeFile path (unlines (header : rows))
    putStrLn $ "Wrote " ++ path
