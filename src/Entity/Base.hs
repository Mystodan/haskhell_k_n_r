module Entity.Base(
  Stats(..),
  Equipment(..),
  EntityBase(..),
  Stat(..),
)where

import GameObjects.Base 
data Stat = Vit | Str | Dex | Res | None deriving (Eq, Show,Enum)
data Stats = Stats {
  vitality ::Int ,
  strength ::Int ,
  dexterity :: Int,
  resilience :: Int
} deriving (Eq,Show)


data Equipment = Equipment{
  rightHand :: HandEquipment ,
  leftHand :: HandEquipment
}deriving (Eq,Show)


data EntityBase = EntityBase{
  name :: String,
  currentHealth :: Int,
  isAlive :: Bool,
  stats :: Stats,
  equipped :: Equipment
} deriving (Eq,Show)