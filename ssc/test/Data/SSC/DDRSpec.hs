module Data.SSC.DDRSpec where

import Test.Hspec
import Data.SSC

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "minfree" $ do
    it "本に載っている例" $ do
      void