module Main (main) where

import Lib

main :: IO ()
main = do
  putStrLn "Hello from smt-all-the-things!"
  someFunc
  demoSMT