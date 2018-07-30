module Mechanics
    ( initState
    , moveObjects
    , Coord(..)
    , Move(..)
    , State(..)
    )
where

import           Data.Maybe
import           Input                          ( Input(..) )
import           Internal

type Coord = (Int, Int)
data Move = Idle
        | Matching
        deriving(Eq)
data State = State { sTopObject :: Coord, sBotObject :: Coord, sMove :: Move, sScore :: Int }

initState :: State
initState = State
    { sTopObject = (0, 0)
    , sBotObject = (screenWidth, screenHeight)
    , sMove      = Idle
    , sScore     = 0
    }

moveObjects :: State -> Maybe Input -> State
moveObjects state@(State _ _ move _) maybeInput
    | move == Idle && isNothing maybeInput = moveSideways state
    | otherwise                            = evalMatch $ moveMatch state

moveSideways :: State -> State
moveSideways (State topObject botObject move score) = State
    { sTopObject = newCoordTop
    , sBotObject = newCoordBot
    , sMove      = move
    , sScore     = score
    }
  where
    newCoordTop = (newX, y)
    newCoordBot = (screenWidth - newX, screenHeight)
    (x, y)      = topObject
    newX        = incObjectHorizontal x

incObjectHorizontal :: Int -> Int
incObjectHorizontal i | i == screenWidth = 0
                      | otherwise        = i + 1

moveMatch :: State -> State
moveMatch (State topObject botObject move score) = State
    { sTopObject = newCoordTop
    , sBotObject = newCoordBot
    , sMove      = newMove
    , sScore     = score
    }
  where
    newCoordTop     = (xT, newY)
    newCoordBot     = (xB, screenHeight - newY)
    (xT  , yT     ) = topObject
    (xB  , _      ) = botObject
    (newY, newMove) = incObjectVertical yT

incObjectVertical :: Int -> (Int, Move)
incObjectVertical i | i == (screenHeight `div` 2) = (0, Idle)
                    | otherwise                   = (i + 1, Matching)

evalMatch :: State -> State
evalMatch (State topObject botObject move score) = State
    { sTopObject = topObject
    , sBotObject = botObject
    , sMove      = move
    , sScore     = newscore
    }
  where
    (xTop, _) = topObject
    (xBot, _) = botObject
    newscore | (move == Idle) && (xTop == xBot) = score + 1
             | move == Matching                 = score
             | otherwise                        = 0
