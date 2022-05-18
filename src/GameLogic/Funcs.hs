module GameLogic.Funcs(
  initPlayer,
  getPlayerName,
  modifyPlayer,
  damagePlayer,
  getStat,
  statUpPlayer,
  zweihandler ,
  entityEquip,
  playerPotEquip,
  playerWepEquip,
  generateStage,
  setHealth,
  getPlayerStatusPlate,
  getCurrentEncounters,
  displayTreasure,
  getCurrentEquipNames,
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
    Player(..),
    PlayerProgress (..),
    PlayerInventory (..),
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
  if rng >= 102 && rng< 103 then   tres --0 and 10
  else if rng >= 0 && rng< 101 then  enem -- 11 and 45
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

playerPotEquip :: Player-> Potion -> Player
playerPotEquip player pot = do
  retval
  where
    retval = modifyPlayerModel player
      (playerBase player)
      (InventoryPotion pot 1)
      (playerProgress player)


playerWepEquip:: Player -> HandEquipment -> String -> Player
playerWepEquip player weapon hand = do
  retval
  where
    retval = modifyPlayerModel player
      (entityEquip (playerBase player) weapon (getHand hand))
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
    retVal = [id,hpBar,equipR, equipL, potEquip, potNum]
    id = ("Name", name (playerBase player) )
    hpBar = ("Health:",setHealth currentHP maxHP)
    currentHP = currentHealth (playerBase player)
    equipped x y = (x,y)
    equipR = equipped "Equipped(r):" eq_r
    equipL = equipped "Equipped(l):" eq_l
    potEquip = equipped "Potion Slot:" (potion_name (getPotValues potSlot))
    potNum =  equipped ("Potion Amount["++ show (level (playerProgress player)) ++"]") (show (getPotValues potAmount))
    getPotValues x =  x (inventory player)
    (eq_r,eq_l) = getCurrentEquipNames player
    maxHP = getEntityHealth (vitality (stats (playerBase player))) (level (playerProgress player))

getCurrentEquipNames:: Player -> (String,String)
getCurrentEquipNames player = (
  if right ==  GameObjects.Base.Weapon (weaponbase right) || right == compareSpecial right  then
    getWeaponName right
  else if right ==  getShield right then
    shield_name right
  else "None",
  if left == GameObjects.Base.Weapon (weaponbase left) || left == compareSpecial left then
    getWeaponName left
  else if left ==  getShield left then
    shield_name left
  else "None")
  where
    (right, left) = getPlayerEquipped player
    compareSpecial x = GameObjects.Base.Special (weaponbase x) (getWeaponEffect x)
    getShield x = GameObjects.Base.Shield (shield_name x) (block x) (shield_rarity x)
    getWeaponEffect = effect
    getWeaponName x = weapon_name(weaponbase x)
    getShieldName = shield_name
    