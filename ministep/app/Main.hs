module Main where

import Lib
import Data.Array
import Data.Foldable

main :: IO ()
main = do
  putSample [L]
  putSample [L, D, U, R]
  putSample [L, D, R, D, L]
  putSample [R, D, L, R, D, L]

putSample :: [Panel] -> IO ()
putSample ps = do
  let input = listArray (1, length ps) ps
  putStrLn $ "input: " ++ show (toList input)
  let output = solve input
  putStrLn $ "output: " ++ show (toList output)
