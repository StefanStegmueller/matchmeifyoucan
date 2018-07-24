module Input
    ( Input(..)
    , getInput
    )
where

import           Control.Concurrent
import           Control.Monad

data Input = Match
    | Exit
    deriving (Eq)

getInput :: MVar Input -> IO ()
getInput var = forever $ do
    char <- getChar
    case char of
        'q' -> putMVar var Exit
        'm' -> putMVar var Match
