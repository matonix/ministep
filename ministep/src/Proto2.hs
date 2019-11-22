{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{- HLINT ignore "Eta reduce" -}
module Proto2 where

import           Data.Traversable
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Maybe
import           Data.Foldable
import           Data.Function

-- # Input/Output related data/type

data Panel = L | D | U | R
  deriving (Show, Eq, Ord, Enum, Bounded, HasEnumAll)
data Foot = FL | FR
  deriving (Show, Eq, Ord, Enum, Bounded, HasEnumAll)

type Panels = [Panel]
type Foots = [Foot]

-- # Internally used data/type

data Note = Note
  { index :: Index
  , panel :: Panel
  } deriving (Show, Eq, Ord)
type Notes = [Note]

type Index = Int
type State = (Note, Foot)
type Action = Foot
type R = Double

class Enum a => HasFromOne a where
  fromOne :: [a]

instance HasFromOne Index where
  fromOne = [1 ..]

class (Enum a, Bounded a) => HasEnumAll a where
  enumAll :: [a]
  enumAll = [minBound..]

class HasInitial a where
  initial :: a

instance HasInitial R where
  initial = 0.0

instance HasInitial Action where
  initial = FL

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
solve panels = toFoots notes $ policyIteration θ γ $ toMDP notes
 where
  notes = zipWith Note [1 ..] panels
  θ     = 0.001
  γ     = 1
  toMDP :: Notes -> MDP
  toMDP notes = MDP states actions transition reward
   where
    -- enumerate all patterns
    states  = notes × enumAll
    actions = enumAll
    -- if (s, a, s') is a valid triple, it gives 1.0
    transition (note, foot) action (note', foot')
      | succ (index note) == index note' && action == foot' = 1
      | otherwise = 0
    -- reward is the reciprocal of cost
    reward s _a s' = 1 / cost s s'
  toFoots :: Notes -> Policy -> Foots
  toFoots notes policy = init $ scanl f initial notes
    where f foot note = policy Map.! (note, foot)

cost :: State -> State -> R
cost (Note _ p, f) (Note _ p', f') = footCost f f'
 where
  footCost x y | x == y    = 2
               | otherwise = 1

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
