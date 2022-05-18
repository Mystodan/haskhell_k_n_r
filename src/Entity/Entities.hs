module Entity.Entities(
  goblin,
  allMobs,
  allBoss,
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
import GameObjects.BaseObjects ( zweihandler, allWeapons )

allMobs :: [Mob]
allMobs = [goblin, zursmann]

allBoss :: [Mob]
allBoss = [ruud]

ruud :: Mob
ruud = defineMob "Rewd" MobType.Default Stats{
  vitality = 3,
  strength = 3,
  dexterity = 3,
  resilience = 3
  } Equipment {
  rightHand = zweihandler,
  leftHand = None
  } 1


goblin::Mob 
goblin = defineMob "Goblin" MobType.Default Stats{
  vitality = 1,
  strength = 1,
  dexterity = 0,
  resilience = 0
  } Equipment {
  rightHand = None,
  leftHand = None
  } 1


zursmann::Mob 
zursmann = defineMob "Zursmann" MobType.Default Stats{
  vitality = 1,
  strength = 0,
  dexterity = 0,
  resilience = 0
  } Equipment {
  rightHand = None,
  leftHand = None
  } 1