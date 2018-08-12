module Input
    ( Input(..)
    , getInput
    )
where

import           Control.Concurrent
import           Control.Monad
import           System.Exit

data Input = Match
    | Restart
    | Exit
    deriving (Eq)

getInput :: MVar Input -> IO ()
getInput var = forever $ do
    char <- getChar
    case char of
        'q' -> do
            putMVar var Exit
            exitSuccess
        'm' -> putMVar var Match
        'r' -> putMVar var Restart
        _   -> return ()
