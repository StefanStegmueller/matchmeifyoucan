module Input
    ( Input(..)
    , getInput
    )
where

import           Control.Concurrent
import           Control.Monad
import           System.Exit
import           System.Console.ANSI

data Input = Match
    | Restart
    deriving (Eq)

getInput :: MVar Input -> IO ()
getInput var = forever $ do
    char <- getChar
    case char of
        'q' -> handleExit
        'm' -> putMVar var Match
        'r' -> putMVar var Restart
        _   -> return ()

handleExit :: IO ()
handleExit = do
    clearScreen
    setCursorPosition 0 0
    exitSuccess
