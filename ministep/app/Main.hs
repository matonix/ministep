module Main where

-- import Proto
import Proto2
import Data.Array
import Data.Foldable

main :: IO ()
main = runProto2

runProto2 :: IO ()
runProto2 = do
  putSample2 [L]
  putSample2 [L, R]
  putSample2 [L, D, R]
  putSample2 [L, D, U, R]
  putSample2 [L, D, R, D, L]
  putSample2 [R, D, L, R, D, L]
  putSample2 [R, D, L, R, U, L]
  putSample2 [L, L, L, L, R, R, R, R]

putSample2 :: [Panel] -> IO ()
putSample2 ps = do
  putStrLn $ "input: " ++ show ps
  let output = solve ps
  putStrLn $ "output: " ++ show output

-- runProto :: IO ()
-- runProto = do
--   putSample [L]
--   putSample [L, D, U, R]
--   putSample [L, D, R, D, L]
--   putSample [R, D, L, R, D, L]

-- putSample :: [Panel] -> IO ()
-- putSample ps = do
--   let input = listArray (1, length ps) ps
--   putStrLn $ "input: " ++ show (toList input)
--   let output = solve input
--   putStrLn $ "output: " ++ show (toList output)