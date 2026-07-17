{- HLINT ignore "Use camelCase" -}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Scrolls.Engine where
import GHC.IsList
import Data.Text

-- Action structure

data Action ix
  -- Control flow
  = A_Seq [Action ix]      -- Sequencing
  | A_Check (Criteria ix)  -- Check a condition
  | A_Isolate (Action ix)  -- Execute the action, revert any changes it makes to the stack
  | A_If (Criteria ix) (Action ix)
  | A_Find (Criteria ix) (Action ix)  -- Find all things on the board matching the criteria, and execute the child action for each one. The thing found will pushed to the stack in each sub-action and popped after.
  -- Board interaction
  | A_Target (Criteria ix) -- Acquire a target (from player input) matching the condition. The target is pushed to the stack.
  | A_CreateWall ix
  | A_Summon UnitClass ix  -- Summon a unit
  | A_GiveCounter CounterKind Int ix  -- Give counters to a unit
  | A_Swap ix ix -- Swap whatever's on two tiles (also used for normal movement)
  | A_Damage ix -- Damage a unit
  -- Card properties
  | A_Persist Int -- Make this card persistent for X turns
  | A_Instant
  -- Other actions
  | A_Draw Int
  | A_GainEnergy Int Int -- max, current
  | A_TimeTrigger Phase (Criteria ix) (Action ix)
  | A_Instruction Text
  | A_CardToZone Zone ix
  | A_ViewZone Zone (Maybe Int) Bool (Action ix)  -- how many cards (Nothing=all), shuffle after

-- Conditions
data Criteria ix
  -- Basic board checks
  = C_IsUnit ix
  | C_IsWall ix
  | C_IsEmpty ix
  | C_IsSpire ix
  | C_IsFriendly ix
  | C_IsVisible ix
  | C_IsVisibleToOpponent ix
  | C_DistanceComparesAs Ordering Int ix ix
  -- Turn and player checks
  | C_MyTurn
  | C_InZone Zone
  -- Resolution stack checks
  | C_IsSpell ix
  -- Composite checks
  | C_Not (Criteria ix)
  | C_And [Criteria ix]
  | C_Succeeds (Action ix)  -- Execute the action (in readonly mode), true iff it succeeds.

data UnitClass = U_Normal | U_Fast | U_Ranged

data CounterKind = Shield | Chain | Stun | Disarm

data Phase = BeginPhase | MainPhase | EndPhase

data Zone
  = Z_Hand
  | Z_Deck
  | Z_Discard
  | Z_Limbo
  | Z_Owner's Zone
  | Z_Opponent's Zone

-- notation

instance Semigroup (Action ix) where
  A_Seq xs <> A_Seq ys = A_Seq (xs++ys)
  x <> A_Seq ys = A_Seq (x:ys)
  A_Seq xs <> y = A_Seq (xs ++ [y])
  x <> y = A_Seq [x, y]

instance Monoid (Action ix) where
  mempty = A_Seq []

instance IsList (Action ix) where
  type Item (Action ix) = Action ix
  fromList = A_Seq
  toList = \case
    A_Seq xs -> xs
    x -> [x]

instance Semigroup (Criteria ix) where
  C_And xs <> C_And ys = C_And (xs ++ ys)
  x <> C_And ys = C_And (x : ys)
  C_And xs <> y = C_And (xs ++ [y])
  x <> y = C_And [x, y]