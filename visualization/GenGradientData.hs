module Main where

import AutoDiff

-- Sample a 2D function and its gradient over a grid, writing a CSV file.
writeGradientField :: String -> (Double, Double) -> Int -> (forall a. Floating a => [a] -> a) -> IO ()
writeGradientField name (lo, hi) n f = do
    let path = "visualization/data/gradients/" ++ name ++ ".csv"
        header = "x,y,f,df_dx,df_dy"
        -- Create domain by interpolating from lo to hi
        domain = [lo + (hi - lo) * fromIntegral i / fromIntegral (n - 1) | i <- [0 .. n - 1]]
        -- Helper to create CSV row of gradient values
        createLine x y = let [gx, gy] = grad f [x, y]
                         in show x ++ "," ++ show y ++ "," ++ show (f [x, y]) ++ "," ++ show gx ++ "," ++ show gy
        rows = [createLine x y | x <- domain, y <- domain]
    writeFile path (unlines (header : rows))

main :: IO ()
main = do
    writeGradientField "sinxcosy" (-pi, pi) 50 (\[x, y] -> sin x * cos y)
