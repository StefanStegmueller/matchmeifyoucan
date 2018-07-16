module Main where

import           Input
import           World
import           System.Console.ANSI
import           System.IO

main :: IO ()
main = do
    hSetEcho stdin False
    hSetBuffering stdin  NoBuffering
    hSetBuffering stdout NoBuffering
    hideCursor
    setTitle "Thieflike"
    gameLoop $ World (0, 0) (0, 0)

gameLoop :: World -> IO ()
gameLoop world@(World hero enemy) = do
    drawHero hero
    drawSymbol enemy 'e'
    input <- getInput
    case input of
        Exit -> handleExit
        _    -> gameLoop $ handleDirection world input

-- when the user wants to exit we give them a thank you
-- message and then reshow the cursor
handleExit :: IO ()
handleExit = do
    clearScreen
    setCursorPosition 0 0
    showCursor
    putStrLn "Thank you for playing!"

