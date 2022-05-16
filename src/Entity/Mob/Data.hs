module Entity.Mob.Data(
  Mob(..),
  MobType(..),
  )where

import Entity.Base(
    Stats(..),
    Equipment(..),
    EntityBase (..),
  )


data MobType = Default | Miniboss | Boss | Ruud deriving (Eq,Show)



data Mob = Mob{
  mobBase ::EntityBase ,
  mobType :: MobType, 
  difficulty:: Int
} deriving (Eq,Show)

