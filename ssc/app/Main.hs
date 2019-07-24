module Main where

import Data.DDR
import Data.SSC 
import RIO

main :: IO ()
main = do
  source <- readFileUtf8  "untracked/Springtime.ssc"
  let Right ssc = decode source
  writeFile "untracked/Springtime_ddr.hs" . show $ fromSSC ssc

