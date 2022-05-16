module Entity.Mob.Functions(defineMob) where

import Entity.Mob.Data(
    Mob(..),
    MobType(..),
  )
import Entity.Player.Data(
    Player(..)
  )
import Entity.Base(
    Stats(..),
    Equipment(..), 
    EntityBase (..),

  )
import Root.Functions(
    getEntityHealth,
  )
import Data.Char ( toLower )

defineMob::String -> MobType -> Stats -> Equipment -> Int -> Mob
defineMob mob_name mob_type stats equipment set =  initMob
  where
    initMob = Mob{
      mobType = mob_type,
      difficulty = set,
      mobBase = initBase
    }
    initBase = EntityBase {
      name = mob_name,
      stats = stats,
      currentHealth = getEntityHealth set (vitality stats),
      isAlive = True,
      equipped = equipment
    }


modifyMob::Mob -> Stats -> Equipment -> Mob
modifyMob mob stats equipment  = initMob
  where
    initMob = Mob{
      mobType = mobType mob,
      difficulty = difficulty mob,
      mobBase = initBase
    }
    initBase = EntityBase {
      name = name $ mobBase mob,
      isAlive = True,
      currentHealth = getEntityHealth (difficulty mob) (vitality stats),
      stats = stats,
      equipped = equipment
    }