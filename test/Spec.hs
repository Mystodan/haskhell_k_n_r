import Test.HUnit (runTestTT, Test (TestCase, TestLabel, TestList), assertEqual)
import GameLogic.Funcs
import Entity.Base
import Entity.Player.Functions
    ( createPlayer, modifyPlayer, modifyPlayerModel )
import Entity.Player.Data
import Entity.Entities
import GameObjects.BaseObjects
import GameObjects.Base
import Root.Functions (getHand)
main :: IO ()
main = do
  _ <- runTestTT unitTests -- UnitTest 
  putStrLn "Testing done!"


unitTests :: Test
unitTests = TestList [
  TestLabel "initPlayer" $ TestCase $
   assertEqual "creates a new player with name"
   (createPlayer "Lars")
   (initPlayer "Lars"),

  TestLabel "restPlayer" $ TestCase $
   assertEqual "restores player after resting"
   (restPlayer (createPlayer "Lars"))
   (restPlayer (initPlayer "Lars")),

  TestLabel "giveXP" $ TestCase $
   assertEqual "gives player XP"
   (giveXP 1 2 (createPlayer "Lars"))
   (giveXP 1 2 (initPlayer "Lars")),

  TestLabel "getPlayerName" $ TestCase $
   assertEqual "gets player's name"
   (getPlayerName (createPlayer "Lars"))
   "Lars",

  TestLabel "modifyPlayer" $ TestCase $ 
   assertEqual "modifies Player values"
   (modifyPlayer (createPlayer "Lars") 1 (stats (playerBase (createPlayer "Lars"))) True (inventory(createPlayer "Lars")) (equipped(playerBase (createPlayer "Lars"))) (playerProgress(createPlayer "Lars")))
   (modifyPlayer (createPlayer "Lars") 1 (stats (playerBase (createPlayer "Lars"))) True (inventory(createPlayer "Lars")) (equipped(playerBase (createPlayer "Lars"))) (playerProgress(createPlayer "Lars"))),

  TestLabel "healPlayer" $ TestCase $
   assertEqual "heals a player"
   (healPlayer 1 (createPlayer "Lars"))
   (healPlayer 1 (createPlayer "Lars")),

  TestLabel "healMob" $ TestCase $
   assertEqual "gets player's current damage"
   (healMob 1 (goblin 1))
   (goblin 1),

  TestLabel "getMobDmg" $ TestCase $
   assertEqual "gets mob's current damage"
   (getMobDmg (goblin 1))
   (getMobDmg (goblin 1)),

  TestLabel "damagePlayer" $ TestCase $
   assertEqual "damages player"
   (damagePlayer (getMobDmg (goblin 1)) (createPlayer "Lars"))
   (damagePlayer (getMobDmg (goblin 1)) (createPlayer "Lars")),

  TestLabel "getStat" $ TestCase $
   assertEqual "converts string to stats"
   (getStat "vit")
   Entity.Base.Vit,

  TestLabel "statUpPlayer" $ TestCase $
   assertEqual "returns player with added stat"
   (statUpPlayer (createPlayer "Lars") "vit")
   (statUpPlayer (createPlayer "Lars") "vit"),

  TestLabel "entityEquip" $ TestCase $
   assertEqual "returns entity with equipped weapon"
   (entityEquip (playerBase(createPlayer "Lars")) (allWeapons!!1) (getHand "r"))
   (entityEquip (playerBase(createPlayer "Lars")) (allWeapons!!1) (getHand "r")),


  TestLabel "playerPotEquip" $ TestCase $
   assertEqual "returns player with potion"
   (playerPotEquip (createPlayer "Lars") lesserHealthPotion)
   (playerPotEquip (createPlayer "Lars") lesserHealthPotion),

  TestLabel "playerWepEquip" $ TestCase $
   assertEqual "returns player with weapon"
   (playerWepEquip (createPlayer "Lars") (allWeapons!!1) "r")
   (playerWepEquip (createPlayer "Lars") (allWeapons!!1) "r"),

  TestLabel "generateStage" $ TestCase $
   assertEqual "generates a random stage"
   (generateStage 1 2)
   (generateStage 1 2),

  TestLabel "setHealth" $ TestCase $
   assertEqual "formats health for print"
   (setHealth  5 10)
   (setHealth  5 10),

  TestLabel "getPlayerStatusPlate" $ TestCase $
   assertEqual "gets player's current status"
   (getPlayerStatusPlate (createPlayer "Lars"))
   (getPlayerStatusPlate (createPlayer "Lars")),

  TestLabel "getCurrentEncounters" $ TestCase $
   assertEqual "gets player's next encounters"
   (getCurrentEncounters 1 [] 3 1)
   (getCurrentEncounters 1 [] 3 1),

  TestLabel "displayTreasure" $ TestCase $
   assertEqual "displays current generated treasures"
   (displayTreasure (Treasure [head allPotions] [head allWeapons]) 0 0 [("","")])
   (displayTreasure (Treasure [head allPotions] [head allWeapons]) 0 0 [("","")]),

  TestLabel "getCurrentEquipNames" $ TestCase $
   assertEqual "gets player's current equips"
   (getCurrentEquipNames (createPlayer "Lars"))
   ("None","None"),

  TestLabel "getHealthAsContainer" $ TestCase $
   assertEqual "gets player's current damage"
   (getHealthAsContainer 9 10)
   (getHealthAsContainer 9 10)

  ]

