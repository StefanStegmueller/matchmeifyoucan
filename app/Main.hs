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
gameLoop var state@(State _ _ _ score) = do
    threadDelay $ fpsToMicSec $ evalFps score
    drawScreen state
    input <- tryTakeMVar var
    handleExit input
    gameLoop var (moveObjects state input)

handleExit :: Maybe Input -> IO ()
handleExit (Just Exit) = do
    clearScreen
    setCursorPosition 0 0
    showCursor
    putStrLn "Thank you for playing!"
    threadId <- myThreadId
    killThread threadId
handleExit _ = return ()

fpsToMicSec :: Int -> Int
fpsToMicSec fps = 1000000 `div` fps

evalFps :: Int -> Int
evalFps 0 = 15
evalFps _ = 15

