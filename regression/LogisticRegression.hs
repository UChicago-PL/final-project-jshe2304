-- | Logistic regression using gradient descent powered by AutoDiff.
--
-- We fit P(y=1|x) = sigmoid(w*x + b) to binary classification data
-- generated from two Gaussians. Gradients are computed automatically
-- via forward-mode AD.
--
-- Usage:  cabal run logistic-regression -- <mu0> <mu1> <sigma>

module Main where

import DataGen (generateBimodalData)
import Train   (train)
import System.Environment (getArgs)

-- | Sigmoid (logistic) function.
sigmoid :: Floating a => a -> a
sigmoid z = 1 / (1 + exp (negate z))

-- | Binary cross-entropy loss for logistic regression.
-- Model: P(y=1|x) = sigmoid(w*x + b), params = [w, b].
bce :: Floating a => [(Rational, Rational)] -> [a] -> a
bce dataset params =
    let w = params !! 0
        b = params !! 1
        n = fromIntegral (length dataset)
        eps = 1e-7
        losses =
            [ negate (fromRational y * log (p + eps) + (1 - fromRational y) * log (1 - p + eps))
            | (x, y) <- dataset
            , let p = sigmoid (w * fromRational x + b)
            ]
    in sum losses / n

-- | Classify a point: 1 if sigmoid(w*x + b) >= 0.5, else 0.
classify :: Double -> Double -> Rational -> Double
classify w b x = if sigmoid (w * fromRational x + b) >= 0.5 then 1 else 0

-- | Compute classification accuracy.
accuracy :: Double -> Double -> [(Rational, Rational)] -> Double
accuracy w b dataset =
    let correct = length [() | (x, y) <- dataset, classify w b x == fromRational y]
    in fromIntegral correct / fromIntegral (length dataset)

main :: IO ()
main = do
    args <- getArgs
    let (mu0, mu1, sigma) = case args of
            [sM0, sM1, sS] -> (read sM0, read sM1, read sS)
            _               -> (-2.0, 2.0, 1.0)
        n = 50
        boundary = (mu0 + mu1) / 2

    let dataset = generateBimodalData mu0 mu1 sigma n

    putStrLn "=== Logistic Regression ==="
    putStrLn ""
    putStrLn $ "mu0 = " ++ show mu0
    putStrLn $ "mu1 = " ++ show mu1
    putStrLn $ "sigma = " ++ show sigma
    putStrLn $ "n = " ++ show n ++ " per class (" ++ show (2 * n) ++ " total)"
    putStrLn $ "boundary = " ++ show boundary
    putStrLn ""

    let params = [0.0, 0.0]
        lr     = 0.1
        steps  = 1000

    putStrLn $ "w_0 = " ++ show (params !! 0)
    putStrLn $ "b_0 = " ++ show (params !! 1)
    putStrLn $ "lr =  " ++ show lr
    putStrLn $ "steps = " ++ show steps
    putStrLn ""
    putStrLn "=== Training ==="

    params' <- train (bce dataset) lr steps params

    let w = params' !! 0
        b = params' !! 1

    putStrLn ""
    putStrLn "=== Results ==="
    putStrLn $ "w_hat = " ++ show w
    putStrLn $ "b_hat = " ++ show b
    putStrLn $ "boundary = " ++ show (negate b / w)
    putStrLn $ "accuracy = " ++ show (accuracy w b dataset * 100) ++ "%"
