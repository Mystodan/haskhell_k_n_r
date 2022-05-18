module IO.GameLoop.Handler(
instanciatePlayer,
gameLoop,
handleEnemyEncounter
) where

import IO.Root.Functions
import Root.Functions
import Constants.GameConstants
import Data.Time ( getCurrentTime )
import Entity.Player.Data
import Stage.Base
import GameLogic.Funcs
import Data.Char (toLower)
import GameObjects.Base
import Control.Concurrent
import GHC.Float.RealFracMethods (roundFloatInt)
import Entity.Base



doubleCheck :: IO Bool
doubleCheck = do
  putStrLn "Are you sure? (y,n)"
  confirm <- getLine
  if not (null confirm) then
    return (toLower(head  confirm) == 'y')
  else doubleCheck


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
  check <- doubleCheck
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
  choice <- promptEncounterChoice amount
  check <- doubleCheck
  if check && choice /= "" then
      return (read choice)
    else
      handleEncounterChoice amount


getEncounter :: Int -> Int -> IO Stage
getEncounter seed amount = do
  choice <- handleEncounterChoice amount
  return $ encounterList!!(choice-1)
  where
    encounterList = getCurrentEncounters [] amount seed

promptEquip :: String -> IO Bool
promptEquip  prompt = do
  putStrLn prompt
  choice <- getLine
  check <- doubleCheck
  if check && toLower (head choice) == 'y' then
    return True
  else
    return False




getWhichEquip ::String -> IO Int
getWhichEquip prompt = do
  putStrLn prompt
  choice <- getLine
  check <- doubleCheck
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
  check <- doubleCheck
  if check && choice /= "" then
    return (head choice)
  else do
    putStrLn "Invalid input"
    getWhichHand prompt

handleEnemyEncounter:: Player -> Bool -> IO Player
handleEnemyEncounter player isPlayerTurn
  | isPlayerTurn && currentHealth (playerBase player) > 0 = do
    putStrLn (getPlayerStatusPlate player)
    handleEnemyEncounter player False
  | otherwise = handleEnemyEncounter player True


handleEncounter :: Player -> Int -> IO Player
handleEncounter player seed = do
  newEncounterDialog
  choice <- getEncounter seed 3
  if choice == Stage.Base.Treasure (treasure choice) then do
    clearScreen
    setDelay
    putStrLn $ "\nYou have encountered: Treasure \n"++ displayTreasure (treasure choice) 0 0 [("","")]
    wChoice <- promptEquip "Do you wish to equip (any of) the weapon(s)?"
    pChoice <- promptEquip "Do you wish to equip (any of) the potion(s)?"
    if wChoice && pChoice then do
      hand <- getWhichHand "Which hand do you wish to Equip the weapon?"
      return (retWep (retPot player choice) choice hand)
    else if wChoice && not pChoice then do
      hand <- getWhichHand "Which hand do you wish to Equip the weapon?"
      return $ retWep player choice hand
    else if not wChoice && pChoice then
      return $ retPot player choice
    else
      return player

  else if choice == Stage.Base.Enemy (encounter choice) then do
    clearScreen
    setDelay
    putStrLn "\nYou have encountered: Enemy"


    return player  else do
    clearScreen
    setDelay
    putStrLn "\nEnountered Rest"
    return player
  where
    retWep x choice hand = playerWepEquip x (weap choice) [hand]
    retPot x choice = playerPotEquip x (pots choice)
    t_equip = treasure
    weap x = head (handEquipment (t_equip x))
    pots x = head (potions (t_equip x))


equipPrompt :: HandEquipment ->IO String
equipPrompt equip = do
  putStrLn ("Do you wish to equip the " ++  weapon_name (weaponbase equip)++"?")
  getLine
handleEquip :: Player -> HandEquipment -> IO Char
handleEquip player equip = do
  choice <- equipPrompt equip
  check <- doubleCheck
  if check && choice /= "" then
     return (head choice)
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
