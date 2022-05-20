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
import Root.Functions

allMobs :: Int -> Int -> [Mob]
allMobs x y = [goblin x, zursmann, zombie x y, slime x y]

allBoss :: Int -> Int  -> [Mob]
allBoss x y = [ruud x y]

calculateDex :: Int -> Int -> Int -> Int
calculateDex min diff seed = genRandNum min diff seed


ruud :: Int -> Int ->Mob
ruud diff seed = defineMob "Rewd" MobType.Ruud Stats{
  vitality = 3,
  strength = 3,
  dexterity = calculateDex 3 dif seed,
  resilience = 3
  } Equipment {
  rightHand = zweihandler diff,
  leftHand = zweihandler diff
  } (dif)
  where
    dif = diff+2



zombie :: Int -> Int -> Mob
zombie diff seed = defineMob "Zombie" MobType.Default Stats{
  vitality = 1,
  strength = 3,
  dexterity = calculateDex 2 dif seed,
  resilience = 2
  } Equipment {
  rightHand = None,
  leftHand = None
  } (dif)
  where
    dif = diff+1


slime :: Int -> Int-> Mob
slime diff seed = defineMob "Slime" MobType.Default Stats{
  vitality = 1,
  strength = 2,
  dexterity = calculateDex 1 diff seed,
  resilience = 0
  } Equipment {
  rightHand = None,
  leftHand = None
  } diff


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
  strength = 1,
  dexterity = 0,
  resilience = 0
  } Equipment {
  rightHand = None,
  leftHand = None
  } 0