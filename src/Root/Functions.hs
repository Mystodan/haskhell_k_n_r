module Root.Functions(
  checkNum,
  modifyEntity,
  damageEntity,
  compareElemToList,
  append,
  genRandNum,
  getEntityHealth,
  allocateStat,
  entityAllocateStat,
  entityEquip,
  getHand,
  getSeed,
  )where

import Constants.GameConstants(
    statToHealth, numPool,
  )
import Entity.Base
    ( Equipment(..),
      Stats(..),
      EntityBase(..),
      Stat(..),
      )
import GameObjects.Base
import Data.Char ( toLower )
import System.Random ( mkStdGen, Random(randomR) )
import Data.Time (UTCTime (utctDayTime))
import Entity.Player.Data


data WeaponHand = Right | Left| None deriving (Show)


append :: a -> [a] -> [a]
append a = foldr (:) [a]

genRandNum:: Int -> Int -> Int -> Int
genRandNum min max seed = retNum
  where
    (retNum,_) = randomR (min,max) $ mkStdGen seed

getHand :: String -> WeaponHand
getHand hand =
  case toLower (head hand) of
    'l' -> Root.Functions.Left
    'r' -> Root.Functions.Right
    _ -> Root.Functions.None

equip :: EntityBase -> WeaponHand -> HandEquipment -> Equipment
equip entityBase hand weapon = do
  let isTwo = isTwohand (weaponbase weapon)
  if isTwo then
    Equipment weapon GameObjects.Base.None
  else
    case hand of
        Root.Functions.Right -> Equipment weapon (leftHand currentHand)
        Root.Functions.Left -> Equipment (rightHand currentHand) weapon
        _ -> currentHand
  where
    currentHand = equipped entityBase



entityEquip :: EntityBase ->  HandEquipment -> WeaponHand -> EntityBase
entityEquip entity weapon hand =
   EntityBase
    (name entity)
    (currentHealth entity)
    (isAlive entity)
    (stats entity)
    (equip entity hand weapon)

modifyEntity :: EntityBase -> Int -> Bool -> Stats -> Equipment -> EntityBase
modifyEntity entity = EntityBase (name entity)

damageEntity:: Int -> EntityBase -> EntityBase
damageEntity damage entity = 
  modifyEntity entity (currentHealth entity-damage) ((currentHealth entity - damage) >= 1) (stats entity) (equipped entity)

getEntityHealth :: Int -> Int-> Int
getEntityHealth vitality level = ((level*3) + (vitality*3)) + statToHealth

allocateStat:: Stats -> Stat -> Stats
allocateStat list val
  | val == Vit = Stats  (vitality list + 1)
                        (strength list)
                        (dexterity list)
                        (resilience list)
  | val == Str = Stats  (vitality list)
                        (strength list + 1)
                        (dexterity list)
                        (resilience list)
  | val == Dex = Stats  (vitality list)
                        (strength list)
                        (dexterity list + 1)
                        (resilience list)
  | val == Res = Stats  (vitality list)
                        (strength list)
                        (dexterity list)
                        (resilience list + 1)
  | otherwise = list

getSeed ::UTCTime -> Int
getSeed currTime = floor $ utctDayTime currTime ::Int

entityAllocateStat::EntityBase -> Stat -> EntityBase
entityAllocateStat entity stat = retVal
  where
    retVal = EntityBase
      (name entity)
      (currentHealth entity)
      (isAlive entity)
      (allocateStat (stats entity) stat)
      (equipped entity)


compareElemToList:: Eq a => a -> [a] -> Bool
compareElemToList  = elem

checkNum :: String -> Int ->  Bool
checkNum list itr
  | itr == length list && compareElemToList (list!!(itr-1)) numPool = True
  | itr < length list && compareElemToList (list!!itr) numPool = checkNum list (itr+1)
  | otherwise = False