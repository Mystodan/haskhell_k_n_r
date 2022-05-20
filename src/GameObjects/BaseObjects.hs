module GameObjects.BaseObjects(
  lesserHealthPotion,
  shieldPotion,
  zweihandler,
  enparakystos,
  shortSword,
  allWeapons,
  allPotions,
)where
import GameObjects.Base


allWeapons :: Int -> [HandEquipment]
allWeapons x = [zweihandler x,  enparakystos, shortSword]

allPotions :: [Potion]
allPotions = [bloodOfGod,greaterHealthPotion ,lesserHealthPotion ,shieldPotion]

zweihandler:: Int -> HandEquipment
zweihandler scale = GameObjects.Base.Weapon{
  weaponbase = BaseWeapon{
    weapon_name = "Zweihandler",
    isTwohand = True,
    attack = (4+scale)`div`2,
    finesse = 0,
    weapon_rarity  = 0.1
  }
}

enparakystos::HandEquipment 
enparakystos = GameObjects.Base.Weapon {
  weaponbase = BaseWeapon{
    weapon_name = "Enparakystos",
    isTwohand = False,
    attack = 1,
    finesse = 1,
    weapon_rarity = 0.1
  }
}

shortSword::HandEquipment 
shortSword = GameObjects.Base.Weapon{
  weaponbase = BaseWeapon{
    weapon_name = "Shortsword",
    isTwohand = False,
    attack = 1,
    finesse = 0,
    weapon_rarity = 0.5
  }
}

lesserHealthPotion::Potion
lesserHealthPotion = Potion{
  potion_name = "Lesser Healing Potion",
  potion_type = GameObjects.Base.Heal{
    isOverTurns = False,
    isFlat = True
  },
  effectivity = 4,
  potion_rarity = 0.5
}

greaterHealthPotion:: Potion
greaterHealthPotion = Potion{
  potion_name = "Greater Healing Potion",
  potion_type = GameObjects.Base.Heal{
    isOverTurns = False,
    isFlat = True
  },
  effectivity = 8,
  potion_rarity = 0.3
}

bloodOfGod::  Potion
bloodOfGod  = Potion{
  potion_name = "Vial(blood of god)",
  potion_type = GameObjects.Base.Heal{
    isOverTurns = False,
    isFlat = False
  },
  effectivity = 75,
  potion_rarity = 0.3
}

shieldPotion:: Potion
shieldPotion = Potion{
  potion_name = "Shield Potion",
  potion_type = GameObjects.Base.Guard,
  effectivity = 1,
  potion_rarity = 0.1
}