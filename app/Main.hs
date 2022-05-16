{-# OPTIONS_GHC -Wall #-}
module Main where

import Root.Functions
import Constants.GameConstants
import Data.Time ( getCurrentTime )
import Entity.Player.Data
import Stage.Base
import Lib
import Data.Char (toLower)
import GameObjects.Base
import Control.Concurrent
import GHC.Float.RealFracMethods (roundFloatInt)

wait :: Float -> IO()
wait x = threadDelay (roundFloatInt(x*1000000.00))

clearScreen :: IO()
clearScreen = putStr newScreen
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
  name <- promptPlayerName
  check <- doubleCheck
  if check && name /= "" then
      return name
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
    setPlayer x y = putStrLn $ (storyInitPlayer ( getPlayerName y))!!x

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

compareElemToList:: Eq a => a -> [a] -> Bool
compareElemToList  = elem

checkNum :: String -> Int ->  Bool
checkNum list itr
  | (itr == length list) && compareElemToList (list!!(itr-1)) numPool = True
  | (itr < length list) && compareElemToList (list!!itr) numPool = checkNum list (itr+1)
  | otherwise = False

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


setDelay :: IO()
setDelay = do
    putStr "."
    wait 0.4
    putStr "."
    wait 0.4
    putStr "."
    wait 0.4
handleEncounter :: Player -> Int -> IO Player
handleEncounter player seed = do
  newEncounterDialog
  choice <- getEncounter seed 3
  if choice == Stage.Base.Treasure (treasure choice) then do
    clearScreen
    setDelay
    putStrLn $ "\nEncountered Treasure \n"++ displayTreasure (treasure choice) 0 0 [("","")]
    wChoice <- promptEquip "Do you wish to equip (any of) the weapon(s)?"
    
 
    pChoice <- promptEquip "Do you wish to equip (any of) the potion(s)?"

    return player
  else if choice == Stage.Base.Enemy (encounter choice) then do
    clearScreen
    setDelay
    putStrLn "\nEnountered Enemy"
    return player
  else do
    clearScreen
    setDelay
    putStrLn "\nEnountered Rest"
    return player



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




main :: IO ()
main = do
  currTime<- getCurrentTime
  let seed = getSeed currTime
  newPlayer <- instanciatePlayer
  _ <- handleEncounter newPlayer seed
  putStrLn ""


