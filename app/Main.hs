{-# OPTIONS_GHC -Wall #-}
module Main where
import Data.Time (getCurrentTime)
import Root.Functions ( getSeed )
import IO.GameLoop.Handler
import Entity.Player.Data
import System.Environment (getArgs)
import Constants.GameConstants
    ( helpText, inappropriateAmountOfFlags, noSuchArgsText, intro, screen ) 


main :: IO ()
main = do
  flags <- getArgs
  if length flags > 1 then
    putStrLn $ inappropriateAmountOfFlags $ length flags  
  else if flags /= [] && (head flags `elem` currFlags) then 
    help
  else if flags /= [] then
    noSuchArgs $ head flags
  else do
    mainMenu
    startGame
  where
    mainMenu = do 
      putStrLn screen   
      putStrLn intro
      _ <- getLine
      putStr "" 
    currFlags = ["--h","--help"] 
    help = putStrLn helpText
    noSuchArgs x = putStrLn $ noSuchArgsText++x
    startGame = do
      currTime<- getCurrentTime
      let seed = getSeed currTime
      newPlayer <- instanciatePlayer
      player <- gameLoop newPlayer seed
      putStrLn $ "Level: "++show( level (playerProgress player))

