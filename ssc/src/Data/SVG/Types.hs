module Data.SVG.Types where

-- Render notes one by one
type Objects = [Object]

data Object
  = Note
      { xPos :: XPos,
        yPos :: YPos,
        direction :: Direction,
        noteType :: NoteType
      }
  | FreezeBar
      { xPos :: XPos,
        yPos :: YPos,
        direction :: Direction,
        freezeLength :: FreezeLength
      }
  | ShockBar
      { xPos :: XPos,
        yPos :: YPos
      }
  | ChangeBPM
      { yPos :: YPos,
        width :: Width,
        bpm :: BPM
      }
  | Stop
      { yPos :: YPos,
        width :: Width,
        stop :: Second
      }
  | Foot
      { xPos :: XPos,
        yPos :: YPos,
        foot :: Foot
      }
  deriving (Eq)

-- Position is scaled by unit size (basically, we use eight beat as unit)
type XPos = Rational

type YPos = Rational

data Direction = L | D | U | R deriving (Eq)

data NoteType = Normal | Freeze | Release | Shock deriving (Eq)

data Foot = FL | FR deriving (Eq)

type FreezeLength = Rational

type BPM = String

type Second = String

type Width = Rational

noteTypeToLayer :: NoteType -> Int
noteTypeToLayer Normal = 1
noteTypeToLayer Freeze = 1
noteTypeToLayer Release = 0
noteTypeToLayer Shock = 1

instance Ord NoteType where
  compare a b = compare (noteTypeToLayer a) (noteTypeToLayer b)

objectToLayer :: Object -> Int
objectToLayer Foot {} = 5
objectToLayer Note {} = 4
objectToLayer FreezeBar {} = 3
objectToLayer ShockBar {} = 2
objectToLayer ChangeBPM {} = 1
objectToLayer Stop {} = 0

instance Ord Object where
  compare a@Note {} b@Note {} = case compare (noteType a) (noteType b) of
    LT -> LT
    GT -> GT
    EQ -> compare (yPos a) (yPos b)
  compare a@FreezeBar {} b@FreezeBar {} = compare (yPos a) (yPos b)
  compare a@ShockBar {} b@ShockBar {} = compare (yPos a) (yPos b)
  compare ChangeBPM {} _ = LT
  compare a b = compare (objectToLayer a) (objectToLayer b)
