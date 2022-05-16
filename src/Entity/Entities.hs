module Entity.Entities(
  goblin,
  allMobs,
) where

import Entity.Base (
  Stats(..),
  Equipment(..),
  )
import  Entity.Mob.Data(
    Mob(..),
    MobType(..),
  )
import GameObjects.Base 
import Entity.Mob.Functions (
  defineMob,
  )
import qualified Entity.Mob.Data as MobType

allMobs :: [Mob]
allMobs = [goblin]

goblin::Mob 
goblin = defineMob "Goblin" MobType.Default Stats{
  vitality = 1,
  strength = 1,
  dexterity = 1,
  resilience = 0
  } Equipment {
  rightHand = None,
  leftHand = None
  } 1


