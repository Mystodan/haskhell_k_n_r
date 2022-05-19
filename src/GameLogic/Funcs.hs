module GameLogic.Funcs(
  giveXP,
  restPlayer,
  initPlayer,
  getPlayerName,
  getPlayerDmg,
  modifyPlayer,
  healPlayer,
  healMob,
  getMobDmg,
  damagePlayer,
  getStat,
  statUpPlayer,
  entityEquip,
  playerPotEquip,
  playerWepEquip,
  generateStage,
  setHealth,
  getPlayerStatusPlate,
  getCurrentEncounters,
  displayTreasure,
  getCurrentEquipNames,
  getHealthAsContainer,
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
import Entity.Mob.Data(
  Mob(..)
  )
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

restPlayer :: Player -> Player
restPlayer player = retval
  where
    retval =
      modifyPlayerModel
      player
      refreshHealth
      refreshPotion
      (playerProgress player)

    refreshHealth = EntityBase
        (name (playerBase player))
        (getEntityHealth (level (playerProgress player))  (vitality (stats (playerBase player))))
        (isAlive (playerBase player))
        (stats (playerBase player))
        (equipped (playerBase player))
    refreshPotion = if potAmount (inventory player) < level (playerProgress player) then
      InventoryPotion (potSlot (inventory player)) (potAmount (inventory player)+1)
      else
        InventoryPotion (potSlot (inventory player)) (potAmount (inventory player))



giveXP :: Int -> Int -> Player -> Player
giveXP seed difficulty player =
    modifyPlayerModel player (playerBase player) (inventory player) (PlayerProgress (level (playerProgress player)) (experiencePts (playerProgress player)+ difficulty+ add))
  where
    add = genRandNum 0 4 seed+10
getRandWeapon :: Int -> HandEquipment
getRandWeapon seed = do
  let rng = genRandNum 0 (length allWeapons-1) seed
  allWeapons!!rng

getRandPotion :: Int -> Potion
getRandPotion seed = do
  let rng = genRandNum 0 (length allPotions-1) seed
  allPotions!!rng


getCurrentEncounters:: Int -> [Stage] -> Int -> Int -> [Stage]
getCurrentEncounters level list amount seed
  | amount > 0 = retval
  | otherwise = list
  where
    initStage = []
    retval = getCurrentEncounters level (append stage list) (amount-1) (seed + 1)
    stage = generateStage level seed

generateStage::Int ->Int -> Stage
generateStage level seed = do
  if rng >= 0 && rng< 10 then   tres --0 and 10
  else if rng >= 11 && rng<= 70 then  enem -- 11 and 45
  else rest
  where
    rng = genRandNum 1 100 seed
    tres = Stage.Base.Treasure {
      treasure = GameObjects.Base.Treasure{
      potions = [getRandPotion seed],
      handEquipment = [getRandWeapon seed]
      }
     }
    bossRng = genRandNum 100 200 (seed+15)
    enem =  Stage.Base.Enemy (if rng >= 100 && rng <= 105 then Stage.Base.Boss randBoss else Stage.Base.Default [randMob] )
    randBoss = allBoss level!!genRandNum 0 (length (allBoss level)-1) (seed+10)
    randMob = allMobs level!!genRandNum 0 (length (allMobs level)-1) (seed+10)
    rest = Stage.Base.Rest

getPlayerName::Player -> String
getPlayerName player = name (playerBase player)

playerPotEquip :: Player-> Potion -> Player
playerPotEquip player pot = do
  retval
  where
    retval = modifyPlayerModel player
      (playerBase player)
      (InventoryPotion pot (potAmount (inventory player)+1))
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
      (PlayerProgress (level (playerProgress player)+1) (experiencePts (playerProgress player)-10))
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
initPlayer name = if name /= "Daniel" then createPlayer name else retVal
  where
    retVal = giveXP 1 10 (createPlayer "Daniel") 
getPot:: Player -> Bool
getPot player = potAmount (inventory player) > 0

healEntity :: Int -> EntityBase -> EntityBase
healEntity heal entity =
  modifyEntity
    entity
    (if healedHP > maxHP  then maxHP else healedHP)
    (isAlive entity)
    (stats entity)
    (equipped entity)
 where
    maxHP = getEntityHealth (vitality (stats entity)) 1
    healedHP = currentHealth entity + heal

healMob :: Int->Mob->Mob
healMob heal mob = retval
  where retval = Mob (healEntity heal (mobBase mob)) (mobType mob) (difficulty mob)


healPlayer :: Int -> Player -> Player
healPlayer heal player = if getPot player then
  modifyPlayer
      player
      (if healedHP > maxHP  then maxHP else healedHP)
      (stats (playerBase player))
      (isAlive (playerBase player))
      (Entity.Player.Data.InventoryPotion (potSlot (inventory player)) (potAmount (inventory player)-1))
      (equipped (playerBase player))
      (playerProgress player)
  else
    player
  where
    maxHP = getEntityHealth (vitality (stats (playerBase player))) (level (playerProgress player))
    healedHP = currentHealth (playerBase player) + heal

getEntityDmg:: EntityBase -> Int
getEntityDmg entity = getDmg
  where
    getWeapon hand
      | hand(equipped entity) == GameObjects.Base.Weapon (weaponbase (hand(equipped entity))) =
      GameObjects.Base.Weapon (weaponbase (hand (equipped entity)))
      | hand(equipped entity) == GameObjects.Base.Special (weaponbase (hand(equipped entity))) NoEff =
      GameObjects.Base.Special (weaponbase (hand (equipped entity))) NoEff
      | otherwise = GameObjects.Base.None

    right = getWeapon rightHand
    left = getWeapon leftHand
    dmg side = if side /= GameObjects.Base.None then attack (weaponbase side) else 0
    getDmg = dmg right + dmg left+strength (stats entity)

getPlayerDmg::Player -> Int
getPlayerDmg player = getEntityDmg(playerBase player)+level (playerProgress player)

getMobDmg :: Mob -> Int
getMobDmg enemy = getEntityDmg(mobBase enemy)+difficulty enemy

damagePlayer::Int -> Player -> Player
damagePlayer damage player =
  if currentHealth (playerBase player) - calculatedDamage < 1 then
    modifyPlayer
      player
      (currentHealth (playerBase player) - calculatedDamage)
      (stats (playerBase player))
      False
      (inventory player)
      (equipped (playerBase player))
      (playerProgress player)
  else
    modifyPlayer
      player
      (currentHealth (playerBase player) - calculatedDamage)
      (stats (playerBase player))
      True
      (inventory player)
      (equipped (playerBase player))
      (playerProgress player)
  where
    armor = resilience (stats (playerBase player))
    calculatedDamage =
      if armor > damage then 0
      else damage - armor



getHealthAsContainer:: Int -> Int -> String
getHealthAsContainer curr max
  | comp > 0 = genBar comp 6
  | otherwise = "{DEAD}"
  where
    comp = fromIntegral curr/fromIntegral max
    genBar percent width = "{" ++ replicate (width-sections percent width) 'Ø' ++ replicate (sections percent width) 'O' ++ "}"
    sections percent width = round (percent * fromIntegral width) :: Int

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
    retVal = [id,hpBar,lvl,equipR, equipL, potEquip, potNum]
    id = ("Name", name (playerBase player) )
    hpBar = ("Health:",setHealth currentHP maxHP)
    currentHP = currentHealth (playerBase player)
    equipped x y = (x,y)
    equipR = equipped "Equipped(r):" eq_r
    equipL = equipped "Equipped(l):" eq_l
    potEquip = equipped "Potion Slot:" (potion_name (getPotValues potSlot))
    potNum =  equipped "Potion Amount" (show (getPotValues potAmount))
    lvl = equipped "Level:" (show (level (playerProgress player)))  
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
    