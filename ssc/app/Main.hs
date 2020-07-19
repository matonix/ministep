module Main where

import Data.DDR
import Data.SSC 
import Data.SVG.DDR
import Data.SVG.Render
import RIO
import RIO.Vector
import Diagrams.Backend.SVG (renderSVG)
import Diagrams.TwoD.Size (mkWidth)

main :: IO ()
main = writeSVG

writeHs :: IO ()
writeHs = do
  source <- readFileUtf8  "untracked/Springtime.ssc"
  let Right ssc = decode source
  writeFile "untracked/Springtime_ddr.hs" . show $ fromSSC ssc

printDDR :: IO ()
printDDR = do
  source <- readFileUtf8  "untracked/NC.sm"
  let Right ssc = decode source
  let ddrs = fromSSC ssc
  let Just ddr = ddrs !? 4
  putStrLn $ prettyPrint ddr

writeSVG :: IO ()
writeSVG = do
  source <- readFileUtf8  "untracked/NC.sm"
  let Right ssc = decode source
  let ddrs = fromSSC ssc
  let Just ddr = ddrs !? 8
  let svg = fromDDR ddr
  renderSVG "untracked/test.svg" (mkWidth 100) $ ddrDiagram svg