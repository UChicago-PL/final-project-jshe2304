module Train (train) where

import AutoDiff (grad)

-- Run gradient descent, printing loss every 100 steps.
train :: (forall a. Floating a => [a] -> a) -> Double -> Int -> [Double] -> IO [Double]
train loss lr steps = trainEpoch 0
  where
    trainEpoch i params
        | i >= steps = return params
        | otherwise = do
            let gradient = grad loss params
                params'  = zipWith (\p g -> p - lr * g) params gradient
            if i `mod` 100 == 0
                then putStrLn $ show i ++ ": " ++ show (loss params')
                else return ()
            trainEpoch (i + 1) params'
