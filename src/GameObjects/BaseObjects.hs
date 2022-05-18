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

allPotions :: [Potion]
allPotions = [lesserHealthPotion,shieldPotion]

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

lesserHealthPotion::Potion
lesserHealthPotion = Potion{
  potion_name = "Lesser Healing Potion",
  potion_type = GameObjects.Base.Heal{
    isOverTurns = False,
    isFlat = True
  },
  effectivity = 1,
  potion_rarity = 0.3
}
shieldPotion::Potion
shieldPotion = Potion{
  potion_name = "Shield Potion",
  potion_type = GameObjects.Base.Guard,
  effectivity = 1,
  potion_rarity = 0.1
}