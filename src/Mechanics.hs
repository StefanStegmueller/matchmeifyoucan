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
moveObjects state@(State _ _ move score) maybeInput
    | move /= Matching && isNothing maybeInput = moveIdle
        state
        (evalIdleMovement score)
    | otherwise = evalMatch $ moveMatch state

evalIdleMovement :: Int -> (Int -> Int)
evalIdleMovement score | score > 4 = \x -> iterate decObjectHorizontal x !! 4
                       | score > 3 = \x -> iterate incObjectHorizontal x !! 4
                       | score > 2 = \x -> iterate decObjectHorizontal x !! 2
                       | score > 1 = \x -> iterate incObjectHorizontal x !! 2
                       | score > 0 = decObjectHorizontal
                       | otherwise = incObjectHorizontal

decObjectHorizontal :: Int -> Int
decObjectHorizontal i | i <= 0    = screenWidth
                      | otherwise = i - 1

incObjectHorizontal :: Int -> Int
incObjectHorizontal i | i >= screenWidth = 0
                      | otherwise        = i + 1

moveIdle :: State -> (Int -> Int) -> State
moveIdle (State topObject botObject move score) moveFunc = State
    { sTopObject = newCoordTop
    , sBotObject = newCoordBot
    , sMove      = move
    , sScore     = score
    }
  where
    newCoordTop = (newX, y)
    newCoordBot = (screenWidth - newX, screenHeight)
    (x, y)      = topObject
    newX        = moveFunc x

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
