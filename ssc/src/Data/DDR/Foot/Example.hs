{-# LANGUAGE RecordWildCards #-}
module Data.DDR.Foot.Example where

import Data.DDR.Types
import qualified RIO.Vector as V
import RIO ( catMaybes )

-- # Input/Output related data/type

-- infiniteLR :: [Foot]
-- infiniteLR = cycle [FL, FR]

example :: Notes -> Notes
example Notes {..} = Notes mode Foot notes'
  where
    notes' = V.map f arrows
    arrows = V.filter (hasArrow . panel . event) notes
    f :: Note -> Note
    f (Note absBeat (ArrowEvent panel _ _)) = Note absBeat (FootEvent foot')
      where
        foot' = fromList . map (fmap (const FL)) $ toList panel
    f (Note absBeat event) = Note absBeat event


mapFoots :: Notes -> [Foot] -> Notes
mapFoots Notes {..} foots = Notes mode Foot notes'
  where
    notes' = V.zipWith f arrows (V.fromListN (V.length arrows) foots)
    arrows = V.filter (hasArrow . panel . event) notes
    f :: Note -> Foot -> Note
    f (Note absBeat (ArrowEvent panel _ _)) foot = Note absBeat (FootEvent foot')
      where
        foot' = fromList . fmap (fmap (const foot)) $ toList panel
    f (Note absBeat event) _ = Note absBeat event

toList :: Panel a -> [Maybe a]
toList (SinglePanel l d u r) = [l, d, u, r]
toList (DoublePanel l1 d1 u1 r1 l2 d2 u2 r2) = [l1, d1, u1, r1, l2, d2, u2, r2]

fromList :: [Maybe a] -> Panel a
fromList [l, d, u, r] = SinglePanel l d u r
fromList [l1, d1, u1, r1, l2, d2, u2, r2] = DoublePanel l1 d1 u1 r1 l2 d2 u2 r2
fromList _ = error "fromList"

hasArrow :: Panel a -> Bool
hasArrow = not . null . catMaybes . toList