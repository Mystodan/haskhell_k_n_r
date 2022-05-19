{-# OPTIONS_GHC -Wall #-}
module Main where
import Data.Time (getCurrentTime)
import Root.Functions ( getSeed )
import IO.GameLoop.Handler
import Entity.Player.Data


main :: IO ()
main = do
  currTime<- getCurrentTime
  let seed = getSeed currTime
  newPlayer <- instanciatePlayer
  player <- gameLoop newPlayer seed
  putStrLn $ "Level: "++show( level (playerProgress player)) 


