{-# OPTIONS_GHC -Wall #-}
module Main where
import Data.Time (getCurrentTime)
import Root.Functions ( getSeed )
import IO.GameLoop.Handler


main :: IO ()
main = do
  currTime<- getCurrentTime
  let seed = getSeed currTime
  newPlayer <- instanciatePlayer
  _ <- gameLoop newPlayer seed
  putStrLn ""


