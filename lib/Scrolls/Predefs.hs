{-# LANGUAGE OverloadedLists #-}
module Scrolls.Predefs where

import Scrolls.Engine
import Data.Foldable

targetTile :: Criteria Int
targetTile = C_IsVisible 0 <> C_IsEmpty 0

targetUnit :: Criteria Int
targetUnit = C_IsVisible 0 <> C_IsUnit 0

friendlyUnit :: Criteria Int
friendlyUnit = C_IsUnit 0 <> C_IsFriendly 0

adjacentTiles :: ix -> ix -> Criteria ix
adjacentTiles = C_DistanceComparesAs EQ 1

standardSummonEffect :: UnitClass -> Int -> Int -> Int -> Action Int
standardSummonEffect cls pos shields chains = fold
  $ [A_Summon cls pos]
  ++ [A_GiveCounter Shield shields pos | shields>0]
  ++ [A_GiveCounter Chain chains pos | chains>0]

standardSummonCard :: UnitClass -> Int -> Int -> Action Int
standardSummonCard cls shields chains = A_Target targetTile <> standardSummonEffect cls 0 shields chains

unseen :: ix -> Criteria ix
unseen = C_Not . C_IsVisibleToOpponent