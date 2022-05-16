module Stage.Base(
  Encounter(..),
  Stage(..),)where
import GameObjects.Base(Treasure)
import Entity.Mob.Data(
  Mob(..),
  MobType(..),
  )

import Entity.Base(
    Stats(..),
    Equipment(..),
    EntityBase (..),
  )

data Encounter = Boss{
  bossEntity :: Mob
} | Default{
  enemies :: [Mob]
} deriving (Eq,Show)


data Stage = Rest | Enemy{
  encounter :: Encounter
} | Treasure{
  treasure :: Treasure 
}  deriving (Eq, Show)
