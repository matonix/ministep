{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Proto2 where

import           Data.Traversable
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Maybe
import           Data.Foldable
import           Data.Function
import qualified Data.Vector                   as V
import           Debug.Trace

-- # Input/Output related data/type

data Panel = L | D | U | R
  deriving (Show, Eq, Ord, Enum, Bounded, HasEnumAll)
data Foot = FL | FR
  deriving (Show, Eq, Ord, Enum, Bounded, HasEnumAll)

type Panels = [Panel]
type Foots = [Foot]

-- # Internally used data/type

type Notes = V.Vector Panel

data FootPos = Weighted
  { curr :: (Foot, Panel)
  , prev :: (Foot, Panel)
  } deriving (Show, Eq, Ord, Bounded, HasEnumAll)
  -- Double (air) has not been implemented yet.

instance (Bounded a, Bounded b, Enum a, Enum b) => Enum (a, b) where
  fromEnum (x, y) = fromEnum x + (1 + fromEnum (maxBound @a)) * fromEnum y
  toEnum n = -- buggy
    let (y, x) = n `divMod` (1 + fromEnum (maxBound @a))
    in  (toEnum x, toEnum y)

instance Enum FootPos where
  fromEnum (Weighted curr prev) = fromEnum (curr, prev)
  toEnum n = let (x, y) = toEnum n in Weighted x y

type Time = Int
type State = (Time, FootPos)
type Action = Foot
type R = Double
type Pos = (R, R)

class (Enum a, Bounded a) => HasEnumAll a where
  enumAll :: [a]
  enumAll = [minBound..maxBound]

class HasInitial a where
  initial :: a

instance HasInitial R where
  initial = 0.0

instance HasInitial Action where
  initial = FL

instance HasInitial Time where
  initial = 0

-- # MDP related data/type

type States = [State]
type Actions = [Action]
type Transition = State -> Action -> State -> R
type Reward = State -> Action -> State -> R

type Value = Map State R
type Policy = Map State Action

data MDP = MDP States Actions Transition Reward

-- # User defined part

solve :: Panels -> Foots
solve panels = toFoots terminal $ policyIteration θ γ $ toMDP notes
 where
  notes    = V.fromList $ L : panels -- add dummy notes
  terminal = V.length notes
  θ        = 0.001
  γ        = 1
  toMDP :: Notes -> MDP
  toMDP notes = MDP states actions transition reward
   where
    -- enumerate all patterns
    states  = [0 .. terminal - 1] × enumAll
    actions = enumAll
    transition s a s' = if nextState s a == s' then 1 else 0
    -- reward is the reciprocal of cost
    reward s a s' = 1 / cost s s'

  -- get next state deterministically
  nextState :: State -> Action -> State
  nextState (time, footPos) action = (nextTime, newFootPos footPos action)
   where
    nextTime = succ time
    nextNote = notes V.! nextTime
    newFootPos (Weighted (f1, p1) (f2, p2)) a
      | f1 == a = Weighted (f1, nextNote) (f2, p2)
      | f1 /= a = Weighted (f2, nextNote) (f1, p1)

  toFoots :: Time -> Policy -> Foots
  toFoots terminal policy = go initialState
   where
    go s@(n, _)
      | n == terminal - 1 -- remove dummy action
      = []
      | otherwise
      = let a = -- traceShow s $ 
                policy Map.! s in a : go (nextState s a)
    initialState = (initial, Weighted (FL, L) (FR, R))

getPos :: Panel -> Pos
getPos L = (-1, 0)
getPos D = (0, -1)
getPos U = (0, 1)
getPos R = (1, 0)

-- TODO: test cost function with small cases 
cost :: State -> State -> R
cost (n, Weighted (f1, p1) (f2, p2)) (n', Weighted (f1', p1') (f2', p2')) =
  centroidW (getPos p1) (getPos p2)
    `distance` centroidW (getPos p1') (getPos p2')
 where
  centroidW (x1, y1) (x2, y2) = ((2 * x1 + x2) / 3, (2 * y1 + y2) / 3)
  distance (x1, x2) (y1, y2) = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

-- # Algorithm part

policyIteration :: R -> R -> MDP -> Policy
policyIteration γ θ mdp@(MDP states actions transition reward) = policyEvalLoop
  initialValue
  initialPolicy
 where
  initialValue :: Value
  initialValue = mkValue states (const initial)
  initialPolicy :: Policy
  initialPolicy = mkPolicy states (const initial)

  policyEvalLoop :: Value -> Policy -> Policy
  policyEvalLoop value policy =
    let p xs ys = (< θ) . sum $ Map.unionWith (\x y -> abs (x - y)) xs ys
        nextValue = whileDiff p $ iterate (policyEvaluation γ mdp) value
    in  policyImproveLoop nextValue policy

  policyImproveLoop :: Value -> Policy -> Policy
  policyImproveLoop value policy =
    let newPolicy = policyImprovement γ mdp value
    in  if newPolicy == policy
          then newPolicy
          else policyEvalLoop value newPolicy

  whileDiff :: (a -> a -> Bool) -> [a] -> a
  whileDiff rel xs = f xs $ tail xs
   where
    f (x : xs) (y : ys) | x `rel` y = x
                        | otherwise = f xs ys

policyEvaluation :: R -> MDP -> Value -> Value
policyEvaluation γ (MDP states actions transition reward) value =
  mkValue states $ \s -> sumWith actions $ \a -> sumWith states
    $ \s' -> transition s a s' * (reward s a s' + γ * (value !. s'))

policyImprovement :: R -> MDP -> Value -> Policy
policyImprovement γ (MDP states actions transition reward) value =
  mkPolicy states $ \s -> argmax actions $ \a -> sumWith states
    $ \s' -> transition s a s' * (reward s a s' + γ * (value !. s'))

-- # Helper functions

argmax :: Ord b => [a] -> (a -> b) -> a
argmax xs f = fst . maximumBy (compare `on` snd) . zip xs $ map f xs

sumWith :: Num b => [a] -> (a -> b) -> b
sumWith xs f = sum $ map f xs

m !. k = fromMaybe 0.0 $ Map.lookup k m

mkValue xs f = Map.fromList . zip xs $ map f xs

mkPolicy xs f = Map.fromList . zip xs $ map f xs

(×) :: [a] -> [b] -> [(a, b)]
(×) xs ys = (,) <$> xs <*> ys
