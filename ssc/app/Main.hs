module Main where

import Data.DDR
import Data.DDR.Foot.Example
import Data.SSC
import Data.SVG.DDR
import Data.SVG.Render
import RIO
import RIO.Vector

main :: IO ()
main = writeSVGWithFoot

writeHs :: IO ()
writeHs = do
  source <- readFileUtf8 "untracked/Springtime.ssc"
  let Right ssc = decode source
  writeFile "untracked/Springtime_ddr.hs" . show $ fromSSC ssc

printDDR :: IO ()
printDDR = do
  source <- readFileUtf8 "untracked/NC.sm"
  let Right ssc = decode source
  let ddrs = fromSSC ssc
  let Just ddr = ddrs !? 4
  putStrLn $ prettyPrint ddr

writeSVG :: IO ()
writeSVG = do
  source <- readFileUtf8 "untracked/ptf.sm"
  let Right ssc = decode source
  let ddrs = fromSSC ssc
  let Just ddr = ddrs !? 8
  let notes = fromDDR ddr
  renderSVG "untracked/test.svg" defaultRenderConfig notes

writeSVGWithFoot :: IO ()
writeSVGWithFoot = do
  source <- readFileUtf8 "untracked/Springtime.ssc"
  let Right ssc = decode source
  let ddrs = fromSSC ssc
  let Just ddr = ddrs !? 0
  let notes = fromDDR ddr
  let foots = fromDDR $ example ddr
  let renderConfig = defaultRenderConfig {sparseX = 1}
  renderSVGWithFoots "untracked/test.svg" renderConfig notes foots
