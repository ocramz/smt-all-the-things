module Lib
    ( someFunc, demoSMT
    ) where

import Data.SBV

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Simple SMT demo: Just show that SBV library is available
demoSMT :: IO ()
demoSMT = do
  putStrLn "Running SMT demo..."
  putStrLn "SBV library successfully imported!"
  -- Simple example: create symbolic variables
  let demo = do
        x <- sInteger "x"
        y <- sInteger "y"
        return (x, y)
  putStrLn "Created symbolic variables x and y"