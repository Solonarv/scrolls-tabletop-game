{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Scrolls.CardFile where

import Scrolls.Card (Prototype(..))
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Scrolls.Engine (Action(..), UnitClass (..), Criteria (..), CounterKind (Shield, Chain, Stun), Phase (..), Zone (..))

import Scrolls.Predefs
import Data.Foldable
import Data.Semigroup

allCards :: Vector Prototype
allCards = Vector.fromList
  [ Prototype{name="Pot of Greed", cost=1, body=[A_Draw 2]}
  , Prototype{name="Pawn", cost=1, body=standardSummonCard U_Normal 0 0}
  , Prototype{name="Soldier", cost=2, body=standardSummonCard U_Normal 1 0}
  , Prototype{name="Knight", cost=4, body=standardSummonCard U_Fast 4 0}
  , Prototype{name="Archer", cost=2, body=standardSummonCard U_Ranged 0 0}
  , Prototype{name="Rat Swarm", cost=3, body=
    [ A_Target targetTile
    , A_Target (targetTile <> adjacentTiles 0 1)
    , A_Target (targetTile <> adjacentTiles 0 2)
    , A_Seq [standardSummonEffect U_Normal n 0 0 | n <- [0,1,2]]
    ]
    }
  , Prototype{name="Pother", cost=2, body=
    [ A_Target targetUnit
    , A_Target (targetTile <> adjacentTiles 0 1)
    , A_Swap 0 1
    , A_Draw 1
    ]}
  , Prototype{name="Transpose", cost=2, body=
    [ A_Target targetUnit
    , A_Target targetUnit
    , A_Check $ C_DistanceComparesAs EQ 1 0 1
    , A_Swap 0 1
    ]}
  , Prototype{name="Imperial Orders", cost=6, body=
    [ A_Draw 4
    , A_GainEnergy 1 0
    , A_Find friendlyUnit (A_GiveCounter Shield 1 0)
    ]}
  , Prototype{name="Hex of the Mire", cost=6, body=
    [ 6 `stimes` A_Target targetUnit
    , fold $ [A_GiveCounter Shield (-6) n <> A_GiveCounter Chain 6 n | n <- [0..5]]
    ]
    }
  , Prototype{name="Thunderstorm", cost=6, body=
    [ A_Find (C_IsUnit 0) (A_Damage 0)
    ]}
  , Prototype{name="Wild Rampage", cost=6, body=
    [ A_Find friendlyUnit (A_Instruction "Move toward closest enemy twice, then attack twice")
    ]
    }
  , Prototype{name="Artillery", cost=6, body= standardSummonCard U_Ranged 4 3}
  , Prototype{name="Wakening Woods", cost=4, body=
    [ A_Persist 4
    , A_TimeTrigger EndPhase C_MyTurn (standardSummonCard U_Normal 1 0)
    ]}
  , Prototype{name="Pinpoint Demolition", cost=2, body=
    [ A_Target (C_IsVisible 0 <> C_IsWall 0)
    , A_Damage 0
    ]
    }
  , Prototype{name="Evasive Maneuver", cost=2, body=
    [ A_Instant
    , A_Target friendlyUnit
    , A_Target (targetTile <> adjacentTiles 0 1)
    , A_Swap 0 1
    ]}
  , Prototype{name="Oaken Armor", cost=2, body=
    [ A_Target friendlyUnit
    , A_GiveCounter Shield 3 0
    , A_GiveCounter Chain 1 0
    ]}
  , Prototype{name="Sleeping Gas", cost=2, body=
    [ A_Target (C_IsVisible 0)
    , A_Find (C_IsUnit 0 <> C_DistanceComparesAs LT 2 0 1) (A_GiveCounter Stun 1 0)
    ]}
  , Prototype{name="Shadow Walk", cost=2, body=
    [ A_Target (friendlyUnit <> unseen 0)
    , A_Target (targetTile <> unseen 0)
    , A_Swap 0 1
    ]}
  , Prototype{name="Animate Wall", cost=3, body=
    [ A_Target (C_IsVisible 0 <> C_IsWall 0)
    , A_Damage 0
    , standardSummonEffect U_Normal 0 3 1
    ]}
  , Prototype{name="Rapid Construction", cost=4, body=
    [ A_Persist 4
    , A_TimeTrigger EndPhase C_MyTurn [A_Target $ C_IsVisible 0 <> C_IsWall 0, A_CreateWall 0]
    ]}
  , Prototype{name="All-Seeing Eye", cost=3, body=
    [A_Persist 2, A_Instruction "You can see all tiles."]}
  , Prototype{name="Remand", cost=3, body=
    [ A_Instant, A_Target (C_IsSpell 0)
    , A_CardToZone (Z_Owner's Z_Hand) 0
    , A_Draw 1
    ]}
  , Prototype{name="Repel", cost=2, body=
    [ A_Target (targetUnit <> C_Not (C_IsFriendly 0))
    , A_Target (C_IsEmpty 0 <> C_Succeeds
        [ A_Find (C_IsSpire 0 <> C_Not (C_IsFriendly 0)) [A_Check $ adjacentTiles 0 1]])
    , A_Swap 0 1
    ]}
  , Prototype{name="Field of Unlife", cost=5, body=A_Instruction "TODO, needs rework anyway"}
  , Prototype{name="Higher Knowledge", cost=3, body=
    [ A_ViewZone Z_Deck Nothing True
      [ A_Target (C_InZone Z_Deck)
      , A_CardToZone Z_Hand 0
      ]
    ]}
  , Prototype{name="Tactical Insight", cost=3, body=A_Instruction "TODO rework?"}
  , Prototype{name="Fireball", cost=4, body=
    [ A_Instant, A_Target (C_IsVisible 0)
    , A_If (C_IsWall 0) (A_Damage 0)
    , A_If (C_IsUnit 0) (2 `stimes` A_Damage 0)
    , A_Find (C_IsUnit 0 <> adjacentTiles 0 1) (A_Damage 0)
    ]}
  , Prototype{name="Living Cannon", cost=4, body=standardSummonCard U_Ranged 4 0}
  ]