module Lib(
  initPlayer,
  getPlayerName,
  modifyPlayer,
  damagePlayer,
  getStat,
  statUpPlayer,
  zweihandler ,
  entityEquip,
  playerEquip,
  generateStage,
  setHealth,
  getPlayerStatusPlate,
  getCurrentEncounters,
  displayTreasure,
   ) where
import Data.Char ( toLower )
import Entity.Entities
import UI.Functions

import Entity.Player.Functions
    ( createPlayer, modifyPlayer, modifyPlayerModel )
import Constants.GameConstants(
  vit,
  str,
  dex,
  res,
 )
import GameObjects.Base
import GameObjects.BaseObjects
import Stage.Base
import Entity.Player.Data(
    Player(..), PlayerProgress (level),
  )
import Entity.Base (
    Stat(..),
    Stats(..) ,
    EntityBase(..) ,
    Equipment (..),
  )
import Root.Functions

{- getMaps::[Stage] -> Int-> Int ->[Stage]
getMaps map seed iter = do -}


getRandWeapon :: Int -> HandEquipment
getRandWeapon seed = do
  let rng = genRandNum 0 (length allWeapons-1) seed
  allWeapons!!rng

getRandPotion :: Int -> Potion
getRandPotion seed = do
  let rng = genRandNum 0 (length allPotions-1) seed
  allPotions!!rng


getCurrentEncounters:: [Stage] -> Int -> Int -> [Stage]
getCurrentEncounters list amount seed
  | amount > 0 = retval
  | otherwise = list
  where
    retval = getCurrentEncounters (append stage list) (amount-1) (seed+10)
    stage = generateStage (seed+10)

generateStage::Int -> Stage
generateStage seed = do
  if rng >= 0 && rng< 33 then   tres
  else if rng >= 33 && rng< 66 then  enem
  else rest
  where
    rng = genRandNum 0 100 seed
    tres = Stage.Base.Treasure {
      treasure = GameObjects.Base.Treasure{
      potions = [getRandPotion seed],
      handEquipment = [getRandWeapon seed]
      }
     }
    enem =  Stage.Base.Enemy $ Default [goblin]
    rest = Stage.Base.Rest

getPlayerName::Player -> String
getPlayerName player = name (playerBase player)

playerEquip:: Player -> HandEquipment -> String -> Player
playerEquip player weapon hand = modifyPlayerModel player
  (entityEquip (playerBase player) zweihandler (getHand "right"))
  (inventory player)
  (playerProgress player)

getStat :: String -> Stat
getStat stat
  | check == head vit || check == vit!!1  = Vit
  | check == head str || check == str!!1  = Str
  | check == head dex || check == dex!!1  = Dex
  | check == head res || check == res!!1  = Res
  | otherwise = Entity.Base.None
  where
    check = map toLower stat

statUpPlayer:: Player -> String -> Player
statUpPlayer player stat = retVal
  where
    retVal = Player
      (entityAllocateStat (playerBase player) (getStat stat))
      (playerProgress player)
      (inventory player)



displayTreasure:: Treasure -> Int->Int -> [(String,String)] -> String
displayTreasure tresr wCount pCount retStr
  | retStr == [("","")] = displayTreasure tresr wCount pCount [row1]
  | wCount == 0 = displayTreasure tresr (wCount+1) pCount (append wRow retStr)
  | wCount < wSize && wCount /= 0 = displayTreasure tresr (wCount+1) pCount (setAppend curWeapon)
  | pCount == 0 = displayTreasure tresr wCount (pCount+1) (append pRow retStr)
  | pCount < pSize && pCount /= 0 = displayTreasure tresr (wCount+1) pCount (setAppend curPotion)
  | otherwise = setContainer retStr 40 0 ""


  where
    setAppend x =  append ("",x) retStr
    row1 = ("Treasure:", "")
    wRow = ("Weapons",curWeapon)
    pRow = ("Potions",curPotion)
    curWeapon = weapon_name (weaponbase (weapons!!wCount))
    curPotion = potion_name (pots!!pCount)
    weapons = handEquipment tresr
    pots = potions tresr
    wSize = length weapons
    pSize = length pots


initPlayer::String -> Player
initPlayer = createPlayer

damagePlayer::Int -> Player -> Player
damagePlayer damage player = do
  if currentHealth (playerBase player) - damage < 1 then
    modifyPlayer
      player
      (currentHealth (playerBase player) - damage)
      (stats (playerBase player))
      False
      (inventory player)
      (equipped (playerBase player))
      (playerProgress player)
  else
    modifyPlayer
      player
      (currentHealth (playerBase player) - damage)
      (stats (playerBase player))
      True
      (inventory player)
      (equipped (playerBase player))
      (playerProgress player)



getHealthAsContainer:: Int -> Int -> String
getHealthAsContainer curr max
  | comp > 0.9 && curr > 0 = "{OOOOOO}"
  | comp > 0.8 && comp <= 0.9 && curr > 0 = "{OOOOOØ}"
  | comp > 0.6 && comp <= 0.8 && curr > 0 = "{OOOOØØ}"
  | comp > 0.4 && comp <= 0.6 && curr > 0 = "{OOOØØØ}"
  | comp > 0.2 && comp <= 0.4 && curr > 0 = "{OOØØØØ}"
  | comp > 0.0 && comp <= 0.2 && curr > 0 = "{OØØØØØ}"
  | otherwise = "{DEAD}"
  where
    comp = fromIntegral curr/fromIntegral max

setHealth::Int -> Int-> String
setHealth curr max = retVal
  where
    spacing = " "
    retVal = "["++show curr ++"/"++ show max++"]"
      ++ ":" ++spacing++ getHealthAsContainer curr max

getPlayerEquipped:: Player -> (HandEquipment,HandEquipment)
getPlayerEquipped player = (rHand,lHand)
  where
    rHand = rightHand (equipped (playerBase player))
    lHand = leftHand (equipped (playerBase player))

getPlayerStatusPlate:: Player -> String
getPlayerStatusPlate player = setContainer retVal 27 0 ""
  where
    retVal = [id,hpBar,equipped]
    id = ("Name", name (playerBase player) )
    hpBar = ("Health:",setHealth currentHP maxHP)
    currentHP = currentHealth (playerBase player)
    equipped = ("Equipped:",show(eq_r ,eq_l))
    (eq_r,eq_l) = getPlayerEquipped player
    maxHP = getEntityHealth (vitality (stats (playerBase player))) (level (playerProgress player))



