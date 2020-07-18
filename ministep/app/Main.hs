module Main where

-- import Proto
import Proto2
import Data.Array
import Data.Foldable

main :: IO ()
main = runProto2

findParamProto2 =
  mapM_ print $ flip filter params $ \p ->
    flip all cases $ \(i, e) -> 
      solveWith (0.01, 1) p i == e

params = (,,,,) <$> ps <*> ps <*> ps <*> ps <*> ps
  where
    -- ps = [1]
    ps = map (/5) [1..5]

cases =
  [ ( [L]
    , [FL]
    )
  , ( [L, R]
    , [FL, FR]
    )
  , ( [L, D, R]
    , [FL, FR, FL]
    )
  , ( [L, D, U, R]
    , [FL, FR, FL, FR]
    )
  , ( [L, D, R, D, L]
    , [FL, FR, FL, FR, FL]
    )
  , ( [R, D, L, R, D, L]
    , [FR, FL, FR, FR, FL, FR]
    -- , [FL, FR, FL, FR, FL, FR]
    )
  , ( [R, D, L, R, U, L]
    , [FR, FL, FR, FL, FR, FL]
    )
  , ( [L, L, L, L, R, R, R, R]
    , [FL, FL, FL, FL, FR, FR, FR, FR]
    )
  ]

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