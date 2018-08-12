module Main where

import           Input
import           Screen
import           Mechanics
import           System.Console.ANSI
import           System.IO
import           Control.Concurrent
import           Control.Monad

main :: IO ()
main = do
    hSetEcho stdin False
    hSetBuffering stdin  NoBuffering
    hSetBuffering stdout NoBuffering
    hideCursor
    setTitle "MatchMeIfYouCan"
    var <- newEmptyMVar
    forkIO $ gameLoop var initState
    getInput var

gameLoop :: MVar Input -> State -> IO ()
gameLoop var state = do
    threadDelay $ fpsToMicSec 15
    drawScreen state
    input <- tryTakeMVar var
    if input == Just Restart
        then gameLoop var initState
        else gameLoop var (moveObjects state input)

fpsToMicSec :: Int -> Int
fpsToMicSec fps = 1000000 `div` fps
