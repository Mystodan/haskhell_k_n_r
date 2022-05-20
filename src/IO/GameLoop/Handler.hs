module IO.GameLoop.Handler(
instanciatePlayer,
gameLoop,
handleEnemyEncounter
) where
import Entity.Mob.Functions
import Combat.Data
import IO.Root.Functions
import Root.Functions
import Constants.GameConstants
import Data.Time ( getCurrentTime )
import Entity.Player.Data
import Stage.Base
import GameLogic.Funcs
import Data.Char (toLower)
import GameObjects.Base
    ( Treasure(handEquipment, potions),
      Potion(potion_name, effectivity, potion_type),
      HandEquipment(weaponbase),
      BaseWeapon(weapon_name), PotionType (isFlat) )
import Control.Concurrent
import GHC.Float.RealFracMethods (roundFloatInt)
import Entity.Base
import Entity.Mob.Data
import Entity.Entities (goblin)
import GameObjects.BaseObjects (lesserHealthPotion)



doubleCheck :: String -> IO Bool
doubleCheck want = do
  putStrLn $ "Are you sure you want to "++want++"? (y,n)"
  confirm <- getLine
  if not (null confirm) then
    return (toLower(head  confirm) == 'y')
  else doubleCheck want

newRestDialog :: IO()
newRestDialog = do
  putStr "\nYou are met with a single campfire"
  wait 0.5
  putStr "."
  wait 0.5
  putStr "."
  wait 0.5
  putStr "."
  wait 0.5
  putStr " Which somehow feels safe..."

newEncounterDialog :: IO ()
newEncounterDialog =  do
  putStr "You seek a new path... yet you only see doors... \n"
  wait 0.6
  putStr  "Yes..."
  wait 0.6
  putStr " you only see doors...\n"
  wait 1.2


promptPlayerName :: IO String
promptPlayerName = do
  putStrLn promptName
  putStr "Enter name: "
  getLine

handlePlayerName :: (IO String)
handlePlayerName = do
  pName <- promptPlayerName
  check <- doubleCheck "finish setting your name"
  if check && pName /= "" then
      return pName
    else
      handlePlayerName

setUpPlayerHandler :: IO Player
setUpPlayerHandler = initPlayer <$> handlePlayerName

instanciatePlayer :: IO Player
instanciatePlayer = do
  setStory 0
  wait 0.6
  setStory 1
  wait 0.6
  player <- setUpPlayerHandler
  clearScreen
  setPlayer 0 player
  wait 0.6
  setPlayer 1 player
  wait 0.6
  putStrLn (getPlayerStatusPlate player)
  return player
  where
    setStory x = putStrLn $ storyIntro!!x
    setPlayer x y = putStrLn $ storyInitPlayer ( getPlayerName y)!!x

promptEncounterChoice :: Int-> IO String
promptEncounterChoice amount = do
  putStrLn $ promptEnounter amount
  putStr "Door:"
  getLine

handleEncounterChoice :: Int -> IO Int
handleEncounterChoice amount = do
  let choices = numPool
  choice <- promptEncounterChoice amount
  check <- doubleCheck "pick this door"
  if check && choice /= "" && head choice `elem`choices then
      retVal (read choice)
    else
      handleEncounterChoice amount
  where
    retVal choice =
      if choice > 0 && choice <= amount then
        return choice
      else handleEncounterChoice amount

getEncounter :: Int -> Int -> Int -> Int -> IO Stage
getEncounter hp level seed amount = do
  choice <- handleEncounterChoice amount
  return $ encounterList!!(choice-1)
  where
    encounterList = getCurrentEncounters hp level [] amount seed

promptEquip :: String -> IO Bool
promptEquip  prompt = do
  putStrLn prompt
  choice <- getLine
  check <- doubleCheck "choose this"
  if check then
    if check && toLower (head choice) == 'y' then
      return True
    else
      return False
  else promptEquip  prompt


getWhichEquip ::String -> IO Int
getWhichEquip prompt = do
  putStrLn prompt
  choice <- getLine
  check <- doubleCheck "equip"
  if check && checkNum choice 0 then
    return $ read choice
  else do
    putStrLn "Invalid input"
    getWhichEquip prompt

getWhichHand :: String -> IO Char
getWhichHand prompt = do
  putStrLn prompt
  putStr "(right or left):"
  choice <- getLine
  check <- doubleCheck "equip to this hand"
  if check && choice /= "" then
    return (head choice)
  else do
    putStrLn "Invalid input"
    getWhichHand prompt

promptPlayerTurn :: Player -> IO Char
promptPlayerTurn player = do
  let choices = ['a','b','p','r']
  putStrLn $ "Player actions:\n\tAttack, Potion:"++fst pot++"["++snd pot++"], Block, Run"
  choice <- getLine

  if choice /= "" && want choice `elem` choices  then
    if inventory player == Entity.Player.Data.InventoryNone && want choice == 'p' then do
      putStrLn "No potion in slot"
      promptPlayerTurn player
    else
      return $ want choice
  else do
    putStrLn "Invalid input"
    promptPlayerTurn player
  where
    pot = if inventory player /= Entity.Player.Data.InventoryNone then
      (potion_name (getPotValues potSlot), show (getPotValues potAmount))
      else ("None","None")
    getPotValues x =  x (inventory player)
    want choice = toLower(head choice)



handlePlayerActions :: Player -> IO Actions
handlePlayerActions player = do
  choice <- promptPlayerTurn player
  check <- doubleCheck $ switchResp choice
  if check then
    if choice == 'a' then
      return Attack
    else if choice == 'b' then do
      putStrLn "Not Implemented Yet!"
      handlePlayerActions player
      --return Block
    else if choice == 'p' then
      return Combat.Data.Potion
    else if choice == 'r' then
      return Combat.Data.Run
    else
      handlePlayerActions player
  else
    handlePlayerActions player
 where
    want = toLower
    switchResp x
      | want x == 'a' = "Attack"
      | want x == 'b' = "Block"
      | want x == 'p' = "use a Potion"
      | want x == 'r' = "Run"
      | otherwise = ""

  
handleEnemyEncounter:: String -> Player -> Mob -> Combat -> Int -> IO Player
handleEnemyEncounter message player enemy combat seed
  | isPlayerTurn combat && currentHealth (playerBase player) > 0 && currentHealth (mobBase enemy) > 0= do
    putStrLn (getPlayerStatusPlate player)
    putStrLn $ "["++name (mobBase enemy)++"]"
    putStrLn message
    putStrLn (name (mobBase enemy)++" HP:"++show(currentHealth (mobBase enemy)))
    playerAct <-  handlePlayerActions player
    case playerAct of
      Combat.Data.Potion ->  getEnemyTurn "You have healed!" (healPlayer hpHeal player) enemy
      Combat.Data.Attack ->  getEnemyTurn "You Have Attacked!" player (damageMob enemy (getPlayerDmg player))
      Combat.Data.Block ->   return player
      Combat.Data.Run -> hasEscape
  | not (isCombat combat) = return player
  | otherwise = do
    putStrLn $ "["++name (mobBase enemy)++"]"
    putStrLn message
    if currentHealth (mobBase enemy) > 0 then
      if enemyRng == 0 then
        getPlayerTurn "The monster healed!" player (healMob mobHeal enemy)
      else
        getPlayerTurn "You were attacked!" (damagePlayer mobAttack player) enemy
    else do
      putStrLn $ "You've Killed The Enemy! EXP: "++ show expgain
      wait 1.5
      getPlayerTurn "The monster died!" (giveXP (seed+5) expgain player) enemy
  where
    expgain = if ((difficulty enemy*3)+ add - (level (playerProgress player)`div`2)) < 0 then
      0 else (difficulty enemy*3)+ add - (level (playerProgress player)`div`2)
    add = genRandNum 0 4 seed+10
    hasEscape = if dexWin then do putStrLn "DODGED!" ; return player else do
      if genRandNum 0 1 seed == 0 then do
        putStrLn "DODGED!"
        return player
      else getEnemyTurn "You Failed to dodge!" player enemy
    dexWin = dexterity (stats (playerBase player)) >= dexterity (stats (mobBase enemy))
    mobAttack = getMobDmg enemy
    getEnemyTurn m p e = handleEnemyEncounter m p e (Combat False True False False) (seed+10)
    getPlayerTurn m p e = handleEnemyEncounter m p e (Combat True False False False) (seed+10)
    hpHeal = if (inventory player) /= Entity.Player.Data.InventoryNone then
        if isFlat (potion_type(potSlot (inventory player))) then
        effectivity (potSlot (inventory player))
        else
          (getEntityHealth (vitality (stats (playerBase player))) (level (playerProgress player))*effectivity (potSlot (inventory player)))`div`100
      else 0
    mobHeal = effectivity (lesserHealthPotion)
    enemyRng = genRandNum 0 19 seed
    redo = handleEnemyEncounter message player enemy combat (seed+10)


handlePlayerTreasChoice:: Stage -> Player -> IO Player
handlePlayerTreasChoice choice player = do
    wChoice <- promptEquip "Do you wish to equip (any of) the weapon(s)?"
    pChoice <- promptEquip "Do you wish to equip (any of) the potion(s)?"
    if wChoice && pChoice then equip1
    else if wChoice && not pChoice then equip2
    else if not wChoice && pChoice then
      return $ retPot player choice
    else
      return player
  where
    equip1 = do
      getH <- getWhichHand "Which hand do you wish to Equip the weapon?"
      if toLower getH `elem` ['r','l'] then 
        return (retWep (retPot player choice) choice getH)
      else equip1
    equip2 = do
      getH <- getWhichHand "Which hand do you wish to Equip the weapon?"
      if toLower getH `elem` ['r','l'] then 
        return $ retWep player choice getH
      else equip2
    t_equip = treasure
    retWep x choice hand = playerWepEquip x (weap choice) [hand]
    retPot x choice = playerPotEquip x (pots choice)
    weap x = head (handEquipment (t_equip x))
    pots x = head (potions (t_equip x))


handleEncounter :: Player -> Int -> IO Player
handleEncounter player seed = do
  newEncounterDialog
  choice <- getEncounter (currentHealth (playerBase player)) (level (playerProgress player)) seed 3
  if choice == Stage.Base.Treasure (treasure choice) then do
    clearScreen
    setDelay
    putStrLn $ "\nYou have encountered: Treasure \n"++ displayTreasure (treasure choice) 0 0 [("","")]
    handlePlayerTreasChoice choice player

  else if choice == Stage.Base.Enemy (encounter choice) then do
    clearScreen
    setDelay
    if  encounter choice == Stage.Base.Boss (bossEntity (encounter choice)) then
      putStrLn "\nYou have encountered: Enemy(boss)"
    else
      putStrLn "\nYou have encountered: Enemy(normal)"
    wait 1.5
    handleEnemyEncounter "" player (enemy choice) (Combat (turn choice) True False False) seed

  else do
    clearScreen
    setDelay
    putStrLn "\nEnountered Rest"
    newRestDialog
    handleRestEncounter player
  where
    enemy x = if encounter x == Stage.Base.Boss (bossEntity (encounter x)) then
       bossEntity (encounter x)
       else
         head (enemies (encounter x))
    turn x = (dexterity (stats (playerBase player)) > dexterity (stats (mobBase (enemy x))))
      || (do
            case genRandNum 0 1 (seed+10) of
              0 -> True
              _ -> False
            )
    retWep x choice hand = playerWepEquip x (weap choice) [hand]
    retPot x choice = playerPotEquip x (pots choice)
    t_equip = treasure
    weap x = head (handEquipment (t_equip x))
    pots x = head (potions (t_equip x))



handleRestEncounter :: Player -> IO Player
handleRestEncounter player = do
  choice <- promptRestEncounter player
  case choice of
    's' -> do
      if levelAmount > 0 then
        statUpHandler player
      else do
        putStr "Not Enough Exp"
        handleRestEncounter player
    'r' -> handleRestPlayer player
    _ -> handleRestEncounter player
  where
    lvl = level (playerProgress player)
    levelAmount = experiencePts (playerProgress player)`div` (10+lvl)
statUpHandler :: Player -> IO Player
statUpHandler player = do
  choice <-statUpPrompt
  case choice of
    'v' -> handleRestEncounter $ statUpPlayer player (head vit)
    'r' -> handleRestEncounter $ statUpPlayer player (head res)
    'd' -> handleRestEncounter $ statUpPlayer player (head dex)
    's' -> handleRestEncounter $ statUpPlayer player (head str)
    _ -> statUpHandler player

statUpPrompt ::  IO Char
statUpPrompt = do
  let choices = ['v','s','d','r']
  putStrLn $ "Player stats:\n\tVitality, Strength, Dexterity, Resilience"
  choice <- getLine
  if (want choice) `elem` choices then do
    check <- doubleCheck $ switchResp choice
    if check then
        return $ want choice
    else 
        statUpPrompt
  else do
    putStrLn "Invalid input"
    statUpPrompt
  where
    want choice = toLower(head choice)
    switchResp x
      | want x == 'v' = "Spec into Vitality"
      | want x == 's' = "Spec into Strength"
      | want x == 'd' = "Spec into Dexterity"
      | want x == 'r' = "Spec into Resilience"
      | otherwise = "ERROR"




handleRestPlayer:: Player -> IO Player
handleRestPlayer player = do

  return $ restPlayer player


promptRestEncounter :: Player -> IO Char
promptRestEncounter player = do
  let choices = ['r','s']
  putStrLn $ "\nPlayer actions:\n\tStat Up ["++show levelAmount++"], Rest"
  choice <- getLine
  check <- doubleCheck $ switchResp choice
  if check && choice /= "" && want choice `elem` choices then
    return (want choice)
  else do
    putStrLn "Invalid input"
    promptRestEncounter player
  where
    lvl = level (playerProgress player)
    levelAmount = experiencePts (playerProgress player)`div` (10+lvl)
    want choice = toLower(head choice)
    switchResp x
      | want x == 's' = "Level Up"
      | otherwise = "Rest"



equipPrompt :: HandEquipment ->IO String
equipPrompt equip = do
  putStrLn ("Do you wish to equip the " ++  weapon_name (weaponbase equip)++"?")
  getLine
handleEquip :: Player -> HandEquipment -> IO Char
handleEquip player equip = do
  choice <- equipPrompt equip
  check <- doubleCheck "equip"
  if check && choice /= "" then
     return (toLower(head choice))
  else
    handleEquip player equip

gameLoop::Player -> Int-> IO Player
gameLoop player seed
  | currentHealth (playerBase player) > 0 = do
    newPlayer <- handleEncounter player seed
    putStrLn $ getPlayerStatusPlate newPlayer
    gameLoop newPlayer (seed+10)
  | otherwise = do
    putStrLn "GAME OVER"
    return player
