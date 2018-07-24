module Screen
        ( Screen(..)
        , initialScreen
        , drawScreen
        , moveObjects
        )
where

import           Input                          ( Input(..) )
import           System.Console.ANSI
import           Data.Maybe

type Coord = (Int, Int)
data State = Idle
        | Matching
        deriving(Eq)
data Screen = Screen { sTopObject :: Coord, sBotObject :: Coord, sState :: State, sMatchCount :: Int }

countObject :: Coord
countObject = (1, 10)

screenWidth :: Int
screenWidth = 80

screenHeight :: Int
screenHeight = 20

initialScreen :: Screen
initialScreen = Screen
        { sTopObject  = (0, 0)
        , sBotObject  = (screenWidth, screenHeight)
        , sState      = Idle
        , sMatchCount = 0
        }

drawScreen :: Screen -> IO ()
drawScreen (Screen topObject botObject state count) = do
        clearScreen
        evalSGR state
        drawLine 0
        drawLine screenHeight
        draw topObject   ['+']
        draw botObject   ['+']
        draw countObject (show count)

evalSGR :: State -> IO ()
evalSGR Idle = setSGR
        [SetConsoleIntensity NormalIntensity, SetColor Foreground Vivid White]
evalSGR Matching = setSGR
        [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red]

moveObjects :: Screen -> Maybe Input -> Screen
moveObjects screen@(Screen _ _ state _) maybeInput
        | state == Idle && isNothing maybeInput = moveSideways screen
        | otherwise                             = evalMatch $ moveMatch screen

moveSideways :: Screen -> Screen
moveSideways (Screen topObject botObject state count) = Screen
        { sTopObject  = newCoordTop
        , sBotObject  = newCoordBot
        , sState      = state
        , sMatchCount = count
        }
    where
        newCoordTop = (newX, y)
        newCoordBot = (screenWidth - newX, screenHeight)
        (x, y)      = topObject
        newX        = incObjectHorizontal x

incObjectHorizontal :: Int -> Int
incObjectHorizontal i | i == screenWidth = 0
                      | otherwise        = i + 1

moveMatch :: Screen -> Screen
moveMatch (Screen topObject botObject state count) = Screen
        { sTopObject  = newCoordTop
        , sBotObject  = newCoordBot
        , sState      = newState
        , sMatchCount = count
        }
    where
        newCoordTop      = (xT, newY)
        newCoordBot      = (xB, screenHeight - newY)
        (xT  , yT      ) = topObject
        (xB  , _       ) = botObject
        (newY, newState) = incObjectVertical yT

incObjectVertical :: Int -> (Int, State)
incObjectVertical i | i == (screenHeight `div` 2) = (0, Idle)
                    | otherwise                   = (i + 1, Matching)

evalMatch :: Screen -> Screen
evalMatch (Screen topObject botObject state count) = Screen
        { sTopObject  = topObject
        , sBotObject  = botObject
        , sState      = state
        , sMatchCount = newCount
        }
    where
        (xTop, _) = topObject
        (xBot, _) = botObject
        newCount | (state == Idle) && (xTop == xBot) = count + 1
                 | state == Matching                 = count
                 | otherwise                         = 0

drawLine :: Int -> IO ()
drawLine y = drawLine' (screenWidth, y)
    where
        drawLine' (0, _) = return ()
        drawLine' (x, y) = do
                draw (x, y) ['-']
                drawLine' (x - 1, y)


draw :: Coord -> String -> IO ()
draw (x, y) symbol = do
        setCursorPosition y x
        putStr symbol

