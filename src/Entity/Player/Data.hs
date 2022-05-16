module Entity.Player.Data(
  PlayerInventory(..),
  Player(..),
  PlayerProgress(..),
)where

import Entity.Base(
    Stats(..),
    Equipment(..),
    EntityBase (..)
  )
import GameObjects.Base

data PlayerInventory = 
  InventoryPotion{
  potSlot :: Potion,
  potAmount :: Int
  }|
  InventoryNone deriving (Show)

data PlayerProgress = PlayerProgress{
  level :: Int,
  experiencePts :: Int
} deriving (Show)

data Player = Player{
  playerBase :: EntityBase, 
  playerProgress :: PlayerProgress,
  inventory :: PlayerInventory
} deriving (Show)