module World
        ( World(..)
        , drawHero
        , drawSymbol
        , handleDirection
        )
where

import           Input                          ( Input(..) )
import           System.Console.ANSI

type Coord = (Int, Int)
data World = World { wHero :: Coord, wEnemy :: Coord}

drawHero :: Coord -> IO ()
drawHero coord = do
        clearScreen
        drawSymbol coord '@'


drawSymbol :: Coord -> Char -> IO ()
drawSymbol (x, y) symbol = do
        setCursorPosition y x
        setSGR
                [ SetConsoleIntensity BoldIntensity
                , SetColor Foreground Vivid Blue
                ]
        putStr [symbol]

-- add the supplied direction to the hero's position, and set that
-- to be the hero's new position, making sure to limit the hero's
-- position between 0 and 80 in either direction
handleDirection :: World -> Input -> World
handleDirection (World hero enemy) input = World
        { wHero  = newHCoord
        , wEnemy = newECood
        }
    where
        newECood       = (80 - newX, 20 - newY)
        newHCoord      = (newX, newY)
        (heroX, heroY) = hero |+| directionToCoord input
        hConst i = max 0 (min i 80)
        newX = hConst heroX
        newY = hConst heroY

-- operator to add 2 coordinates together
(|+|) :: Coord -> Coord -> Coord
(|+|) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

directionToCoord :: Input -> Coord
directionToCoord d | d == Up     = (0, -1)
                   | d == Down   = (0, 1)
                   | d == Left'  = (-1, 0)
                   | d == Right' = (1, 0)
                   | otherwise   = (0, 0)
