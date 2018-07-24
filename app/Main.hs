module Main where

import           Input
import           Screen
import           System.Console.ANSI
import           System.IO
import           Control.Concurrent
import           Control.Monad

main :: IO ()
main = do
    hSetEcho stdin False
    hSetBuffering stdin  NoBuffering
    hSetBuffering stdout (BlockBuffering (Just (80 * 20)))
    hideCursor
    setTitle "MatchMeIfYouCan"
    var <- newEmptyMVar
    forkIO $ gameLoop var initialScreen
    getInput var

gameLoop :: MVar Input -> Screen -> IO ()
gameLoop var screen@(Screen _ _ _ score) = do
    threadDelay $ fpsToMicSec $ evalSpeed score
    drawScreen screen
    input <- tryTakeMVar var
    handleExit input
    gameLoop var (moveObjects screen input)

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

evalSpeed :: Int -> Int
evalSpeed 0     = 15
evalSpeed score = 15 * (score + 1)

