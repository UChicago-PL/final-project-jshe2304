-- | Simple linear regression using gradient descent powered by AutoDiff.
--
-- We fit y = m*x + b to noisy data sampled from a given function on [-4, 4].
-- Gradients are computed automatically via forward-mode AD.
--
-- Usage:  cabal run linear-regression -- <slope> <intercept>

module Main where

import DataGen (generateLinearData)
import Train   (train)
import System.Environment (getArgs)

-- | Mean squared error for a linear model y = m*x + b.
mse :: Fractional a => [(Rational, Rational)] -> [a] -> a
mse dataset params =
    let m = params !! 0
        b = params !! 1
        n = fromIntegral (length dataset)
        errs = [ (fromRational y - (m * fromRational x + b)) ^ (2 :: Int) | (x, y) <- dataset ]
    in sum errs / n

main :: IO ()
main = do
    args <- getArgs
    let (m, b) = case args of
            [sM, sB] -> (read sM, read sB)
            _        -> (3.0, 1.0)
        f x = m * x + b
        sigma = 0.5
        n = 50

    let dataset = generateLinearData f (-4) 4 n sigma

    putStrLn "=== Linear Regression ==="
    putStrLn ""
    putStrLn $ "m = " ++ show m
    putStrLn $ "b = " ++ show b
    putStrLn $ "sigma = " ++ show sigma
    putStrLn $ "n = " ++ show n ++ " on [-4, 4]"
    putStrLn ""

    let params = [0.0, 0.0]
        lr = 0.001
        steps = 1000

    putStrLn $ "m_0 = " ++ show (params !! 0)
    putStrLn $ "b_0 = " ++ show (params !! 1)
    putStrLn $ "lr =  " ++ show lr
    putStrLn $ "steps = " ++ show steps
    putStrLn ""
    putStrLn "=== Training ==="

    params' <- train (mse dataset) lr steps params

    let m' = params' !! 0
        b' = params' !! 1

    putStrLn ""
    putStrLn "=== Results ==="
    putStrLn $ "m_hat = " ++ show m'
    putStrLn $ "b_hat = " ++ show b'