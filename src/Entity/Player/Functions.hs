module Entity.Player.Functions(
  createPlayer,
  modifyPlayer,
  modifyPlayerModel,
  ) where

import Entity.Base(
    Stats(..),
    Equipment (..),
    EntityBase (..),
  )
import Root.Functions(
    getEntityHealth,
  )

import GameObjects.BaseObjects(
    lesserHealthPotion,
    shieldPotion,
  )
import GameObjects.Base
import Entity.Player.Data(
    PlayerInventory(..),
    Player(..),
    PlayerProgress(..)
  )



createPlayer::String -> Player
createPlayer inn_name =  initPlayer
  where
    initStatus = Stats{
        vitality = 1,
        strength = 1,
        dexterity = 1,
        resilience = 0
      }
    initInventory = InventoryPotion{
      potSlot = lesserHealthPotion,
      potAmount = 1
    }
    initEquipped = Equipment {
      rightHand = None ,
      leftHand = None
    }
    initProgress = PlayerProgress {
      experiencePts = 0,
      level = 1
    }
    initEntity = EntityBase {
      name = inn_name,
      currentHealth = getEntityHealth (level initProgress)  (vitality initStatus),
      stats = initStatus,
      isAlive = True,
      equipped = initEquipped
    }
    initPlayer = Player{
      playerProgress = initProgress,
      inventory = initInventory,
      playerBase = initEntity
    }





modifyPlayer:: Player -> Int -> Stats -> Bool ->PlayerInventory -> Equipment -> PlayerProgress ->Player
modifyPlayer player currHP stats isAlive inventory equipment progress  = initPlayer
  where
    initPlayer = Player{
      playerBase = initEntity,
      playerProgress = progress,
      inventory = inventory
    }
    initEntity = EntityBase {
      name = name $ playerBase player,
      currentHealth = currHP,
      stats = stats,
      isAlive = isAlive,
      equipped = equipment
    }

modifyPlayerModel:: Player -> EntityBase -> PlayerInventory -> PlayerProgress ->Player
modifyPlayerModel player baseinfo inventory progress  = initPlayer
  where
    initPlayer = Player{
      playerBase = baseinfo,
      playerProgress = progress,
      inventory = inventory
    }


