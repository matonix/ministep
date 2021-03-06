{-# LANGUAGE TypeApplications #-}

module Data.SVG.DDR where

import qualified Data.DDR.Types as D
import Data.Ratio ((%))
import Data.SVG.Types
import RIO (Arrow ((&&&)))
import qualified RIO.Vector as V

fromDDR :: D.Notes -> Objects
fromDDR (D.Notes mode D.Arrow notes) =
  let notes' = V.toList notes
   in concatMap fromNotes notes'
        ++ mkShockBars mode notes'
        ++ mkFeeezeBars mode notes'
        ++ mkChangeBPMs mode notes'
        ++ mkStops mode notes'
fromDDR (D.Notes _mode D.Foot notes) =
  let notes' = V.toList notes
   in concatMap fromNotes notes'

-- a DDR note has some SVG notes
fromNotes :: D.Note -> Objects
fromNotes (D.Note absBeat event) =
  let pos = fromAbsBeat absBeat
   in case event of
        D.ArrowEvent panel _ _ -> fromArrow panel pos
        D.FootEvent panel -> fromFoot panel pos

fromAbsBeat :: D.AbsBeat -> YPos
fromAbsBeat (D.AbsBeat measure denominator numerator) =
  fromIntegral measure + fromIntegral numerator % fromIntegral denominator

fromArrow :: D.Panel D.NoteType -> YPos -> Objects
fromArrow panel y =
  let directions = cycle [L, D, U, R]
   in [ Note x y dir (fromNoteType nt)
        | (x, dir, Just nt) <- zip3 [0 ..] directions $ toList panel
      ]

toList :: D.Panel a -> [Maybe a]
toList (D.SinglePanel l d u r) = [l, d, u, r]
toList (D.DoublePanel l1 d1 u1 r1 l2 d2 u2 r2) = [l1, d1, u1, r1, l2, d2, u2, r2]

fromNoteType :: D.NoteType -> NoteType
fromNoteType D.Normal = Normal
fromNoteType D.Freeze = Freeze
fromNoteType D.Release = Release
fromNoteType D.Shock = Shock

fromFoot :: D.Panel D.Foot -> YPos -> Objects
fromFoot panel y =
  [ Foot x y (fromFootType f)
    | (x, Just f) <- zip [0 ..] $ toList panel
  ]

fromFootType :: D.Foot -> Foot
fromFootType D.FL = FL
fromFootType D.FR = FR

mkFeeezeBars :: D.Mode -> [D.Note] -> Objects
mkFeeezeBars D.Single notes =
  let lanes = [D.left, D.down, D.up, D.right]
      directions = [L, D, U, R]
   in concat
        [ mkFreezeBarByLane x dir lane notes
          | (x, dir, lane) <- zip3 [0 ..] directions lanes
        ]
mkFeeezeBars D.Double notes =
  let lanes =
        [ D.left1p,
          D.down1p,
          D.up1p,
          D.right1p,
          D.left2p,
          D.down2p,
          D.up2p,
          D.right2p
        ]
      directions = [L, D, U, R, L, D, U, R]
   in concat
        [ mkFreezeBarByLane x dir lane notes
          | (x, dir, lane) <- zip3 [0 ..] directions lanes
        ]

mkFreezeBarByLane ::
  XPos -> Direction -> (D.Panel D.NoteType -> Maybe D.NoteType) -> [D.Note] -> [Object]
mkFreezeBarByLane x dir lane notes =
  let arrows =
        [ (ab, nt)
          | (ab, Just nt) <- map (D.absBeat &&& lane . D.panel . D.event) notes
        ]
   in [FreezeBar x (y + len / 2) dir len | (y, len) <- mkFreezeReleasePair arrows]

mkFreezeReleasePair :: [(D.AbsBeat, D.NoteType)] -> [(YPos, FreezeLength)]
mkFreezeReleasePair = go [] Nothing
  where
    go acc _ [] = acc
    go acc Nothing ((a, D.Freeze) : xs) = go acc (Just a) xs
    go acc (Just a) ((b, D.Release) : xs) =
      go ((fromAbsBeat a, calcFreezeLength a b) : acc) Nothing xs
    go acc x (_ : xs) = go acc x xs

calcFreezeLength :: D.AbsBeat -> D.AbsBeat -> FreezeLength
calcFreezeLength a b = fromAbsBeat b - fromAbsBeat a

mkShockBars :: D.Mode -> [D.Note] -> Objects
mkShockBars D.Single notes = mkShockBars' (0 + 1.5) D.left notes
mkShockBars D.Double notes =
  mkShockBars' (0 + 1.5) D.left1p notes ++ mkShockBars' (4 + 1.5) D.left2p notes

mkShockBars' :: XPos -> (D.Panel D.NoteType -> Maybe D.NoteType) -> [D.Note] -> Objects
mkShockBars' x lane notes =
  [ ShockBar x (fromAbsBeat ab)
    | (ab, Just D.Shock) <- map (D.absBeat &&& lane . D.panel . D.event) notes
  ]

mkChangeBPMs :: D.Mode -> [D.Note] -> Objects
mkChangeBPMs D.Single notes = mkChangeBPMs' 4 notes
mkChangeBPMs D.Double notes = mkChangeBPMs' 8 notes

mkChangeBPMs' :: Width -> [D.Note] -> Objects
mkChangeBPMs' len notes =
  [ ChangeBPM (fromAbsBeat ab) len (format b)
    | (ab, Just b) <- map (D.absBeat &&& D.changeBPM . D.event) notes
  ]
  where
    format :: D.BPM -> BPM
    format x = if x == fromInteger (round x) then show @Int $ round x else show x

mkStops :: D.Mode -> [D.Note] -> Objects
mkStops D.Single notes = mkStops' 4 notes
mkStops D.Double notes = mkStops' 8 notes

mkStops' :: Width -> [D.Note] -> Objects
mkStops' len notes =
  [ Stop (fromAbsBeat ab) len (format b)
    | (ab, Just b) <- map (D.absBeat &&& D.stop . D.event) notes
  ]
  where
    format :: D.Second -> Second
    format x =
      if x == fromInteger (round x)
        then show @Int (round x) ++ "s"
        else show x ++ "s"
