module Data.SVG.DDR where

import           RIO
import qualified RIO.Vector                    as V

import qualified Data.DDR.Types                as D
import           Data.SVG.Types
import           Data.Ratio                     ( (%) )

fromDDR :: D.Notes -> Objects
fromDDR (D.Notes mode _rep notes) =
  let notes' = V.toList notes
  in  concatMap fromNotes notes'
        ++ mkShockBars mode notes'
        ++ mkFeeezeBars mode notes'
-- fromDDR (D.Notes _mode _rep _notes) = undefined

-- a DDR note has some SVG notes
fromNotes :: D.Note -> Objects
fromNotes (D.Note absBeat event) =
  let
    -- TODO: changeBPM and stop
      D.Event panel _changeBPM _stop = event
      pos                            = fromAbsBeat absBeat
  in  fromPanel panel pos

fromAbsBeat :: D.AbsBeat -> YPos
fromAbsBeat (D.AbsBeat measure denominator numerator) =
  fromIntegral measure + fromIntegral numerator % fromIntegral denominator

fromPanel :: D.Panel -> YPos -> Objects
fromPanel (D.SinglePanel l d u r) y =
  let directions = [L, D, U, R]
  in  [ Note x y dir (fromNoteType nt)
      | (x, dir, Just nt) <- zip3 [0 ..] directions [l, d, u, r]
      ]
fromPanel (D.DoublePanel l1 d1 u1 r1 l2 d2 u2 r2) y =
  let directions = [L, D, U, R, L, D, U, R]
  in  [ Note x y dir (fromNoteType nt)
      | (x, dir, Just nt) <- zip3 [0 ..]
                                  directions
                                  [l1, d1, u1, r1, l2, d2, u2, r2]
      ]

fromNoteType :: D.NoteType -> NoteType
fromNoteType D.Normal  = Normal
fromNoteType D.Freeze  = Freeze
fromNoteType D.Release = Release
fromNoteType D.Shock   = Shock

mkFeeezeBars :: D.Mode -> [D.Note] -> Objects
mkFeeezeBars D.Single notes =
  let lanes      = [D.left, D.down, D.up, D.right]
      directions = [L, D, U, R]
  in  concat
        [ mkFreezeBarByLane x dir lane notes
        | (x, dir, lane) <- zip3 [0 ..] directions lanes
        ]
mkFeeezeBars D.Double notes =
  let lanes =
          [ D.left1p
          , D.down1p
          , D.up1p
          , D.right1p
          , D.left2p
          , D.down2p
          , D.up2p
          , D.right2p
          ]
      directions = [L, D, U, R, L, D, U, R]
  in  concat
        [ mkFreezeBarByLane x dir lane notes
        | (x, dir, lane) <- zip3 [0 ..] directions lanes
        ]

mkFreezeBarByLane
  :: XPos -> Direction -> (D.Panel -> Maybe D.NoteType) -> [D.Note] -> [Object]
mkFreezeBarByLane x dir lane notes =
  let arrows =
          [ (ab, nt)
          | (ab, Just nt) <- map (D.absBeat &&& lane . D.panel . D.event) notes
          ]
  in  [ FreezeBar x (y + len / 2) dir len | (y, len) <- mkFreezeReleasePair arrows ]

mkFreezeReleasePair :: [(D.AbsBeat, D.NoteType)] -> [(YPos, FreezeLength)]
mkFreezeReleasePair = go [] Nothing
 where
  go acc _       []                   = acc
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

mkShockBars' :: XPos -> (D.Panel -> Maybe D.NoteType) -> [D.Note] -> Objects
mkShockBars' x lane notes =
  [ ShockBar x (fromAbsBeat ab)
  | (ab, Just D.Shock) <- map (D.absBeat &&& lane . D.panel . D.event) notes
  ]
