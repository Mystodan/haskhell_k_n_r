module IO.Root.Functions(
wait,
clearScreen,
setDelay
) where
import GHC.Float.RealFracMethods (roundFloatInt)
import Control.Concurrent (threadDelay)
import Constants.GameConstants (newScreen)

wait :: Float -> IO()
wait x = threadDelay (roundFloatInt(x*1000000.00))

clearScreen :: IO()
clearScreen = putStr newScreen

setDelay :: IO()
setDelay = do
    putStr "."
    wait 0.4
    putStr "."
    wait 0.4
    putStr "."
    wait 0.4