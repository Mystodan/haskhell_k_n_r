module GameObjects.Base(
    HandEquipment(..),
    PotionType(..),
    Potion(..),
    Treasure(..),
    BaseWeapon(..),
    EffectType(..),
  )where

data EffectType = Leech | Frost deriving (Eq,Show)

data BaseWeapon = BaseWeapon{
  weapon_name :: String, 
  isTwohand :: Bool,
  attack :: Int,
  finesse :: Int,
  weapon_rarity :: Float
}deriving (Eq,Show)

data HandEquipment = None | Weapon{
  weaponbase::BaseWeapon 
}| Shield{
  shield_name :: String, 
  block :: Int ,
  shield_rarity :: Float
}|Special {
  weaponbase::BaseWeapon,
  effect :: EffectType
}deriving (Eq,Show)

data PotionType =
    Heal  {
  isOverTurns:: Bool,
  isFlat :: Bool 
  } 
  | Guard deriving (Eq,Show)

data Potion = Potion{
  potion_name :: String,
  potion_type :: PotionType,
  effectivity :: Int,
  potion_rarity :: Float
} deriving(Eq,Show)

data Treasure = Treasure{
    potions :: [Potion],
    handEquipment :: [HandEquipment]
} deriving (Eq,Show)