module Input
    ( Input(..)
    , getInput
    )
where

import           System.IO

data Input = Up
    | Down
    | Left'
    | Right'
    | Exit
    deriving (Eq)

getInput :: IO Input
getInput = do
    char <- getChar
    case char of
        'q' -> return Exit
        'w' -> return Up
        's' -> return Down
        'a' -> return Left'
        'd' -> return Right'
        _   -> getInput
