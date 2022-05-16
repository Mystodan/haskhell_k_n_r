module Root.Functions(
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
    statToHealth,
  )
import Entity.Base
    ( Equipment(Equipment),
      Stats(..),
      EntityBase(..),
      Stat(..),
      )
import GameObjects.Base 
import Data.Char ( toLower )
import System.Random ( mkStdGen, Random(randomR) )
import Data.Time (UTCTime (utctDayTime))


data WeaponHand = Right | Left| None deriving (Show)


append :: a -> [a] -> [a]
append a = foldr (:) [a]

genRandNum:: Int -> Int -> Int -> Int
genRandNum min max seed = retNum
  where 
    (retNum,_) = randomR (min,max) $ mkStdGen seed

getHand :: String -> WeaponHand 
getHand hand = 
  case map toLower hand of
    "left" -> Root.Functions.Left
    "right" -> Root.Functions.Right
    _ -> Root.Functions.None

equip :: WeaponHand -> HandEquipment -> Equipment  
equip hand weapon = do
  let isTwo = isTwohand (weaponbase weapon)
  if isTwo then 
    Equipment weapon GameObjects.Base.None 
  else
    case hand of
        Root.Functions.Left -> Equipment weapon GameObjects.Base.None 
        Root.Functions.Right -> Equipment GameObjects.Base.None weapon
        _ -> Equipment GameObjects.Base.None GameObjects.Base.None

entityEquip :: EntityBase ->  HandEquipment -> WeaponHand -> EntityBase 
entityEquip entity weapon hand =
   EntityBase 
    (name entity) 
    (currentHealth entity) 
    (isAlive entity) 
    (stats entity) 
    (equip hand weapon)

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

