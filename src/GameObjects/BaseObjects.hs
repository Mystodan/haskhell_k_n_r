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


allWeapons :: [HandEquipment]
allWeapons = [zweihandler, enparakystos, shortSword]

allPotions :: Int -> [Potion]
allPotions x = [bloodOfGod x,greaterHealthPotion x,lesserHealthPotion x,shieldPotion x]

zweihandler::HandEquipment
zweihandler = GameObjects.Base.Weapon{
  weaponbase = BaseWeapon{
    weapon_name = "Zweihandler",
    isTwohand = True,
    attack = 2,
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

lesserHealthPotion::Int ->Potion
lesserHealthPotion currH = Potion{
  potion_name = "Lesser Healing Potion",
  potion_type = GameObjects.Base.Heal{
    isOverTurns = False,
    isFlat = True
  },
  effectivity = 4,
  potion_rarity = 0.5
}

greaterHealthPotion:: Int -> Potion
greaterHealthPotion currH = Potion{
  potion_name = "Greater Healing Potion",
  potion_type = GameObjects.Base.Heal{
    isOverTurns = False,
    isFlat = True
  },
  effectivity = 8,
  potion_rarity = 0.3
}

bloodOfGod:: Int -> Potion
bloodOfGod currH = Potion{
  potion_name = "Vial(blood of god)",
  potion_type = GameObjects.Base.Heal{
    isOverTurns = False,
    isFlat = True
  },
  effectivity = round(fromIntegral ((currH * 7)`div` 10))::Int,
  potion_rarity = 0.3
}

shieldPotion::Int ->Potion
shieldPotion currH= Potion{
  potion_name = "Shield Potion",
  potion_type = GameObjects.Base.Guard,
  effectivity = 1,
  potion_rarity = 0.1
}