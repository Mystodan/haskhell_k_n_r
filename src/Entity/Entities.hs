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

allMobs :: Int -> [Mob]
allMobs x = [goblin x, zursmann, zombie x, slime x]

allBoss :: Int -> [Mob]
allBoss x = [ruud x]


ruud :: Int -> Mob
ruud diff = defineMob "Rewd" MobType.Ruud Stats{
  vitality = 3,
  strength = 3,
  dexterity = 3,
  resilience = 3
  } Equipment {
  rightHand = zweihandler,
  leftHand = None
  } (diff+2)

zombie :: Int -> Mob
zombie diff = defineMob "Zombie" MobType.Default Stats{
  vitality = 1,
  strength = 3,
  dexterity = 1,
  resilience = 3
  } Equipment {
  rightHand = None,
  leftHand = None
  } (diff+1)

slime :: Int -> Mob
slime = defineMob "Slime" MobType.Default Stats{
  vitality = 1,
  strength = 2,
  dexterity = 1,
  resilience = 0
  } Equipment {
  rightHand = None,
  leftHand = None
  }


goblin::Int -> Mob
goblin = defineMob "Goblin" MobType.Default Stats{
  vitality = 1,
  strength = 1,
  dexterity = 0,
  resilience = 0
  } Equipment {
  rightHand = None,
  leftHand = None
  }


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