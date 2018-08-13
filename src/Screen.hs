module Screen
        ( drawScreen
        , screenWidth
        , screenHeight
        )
where

import           System.Console.ANSI
import           Mechanics
import           Internal

textPosition :: Coord
textPosition = (1, 9)

levelDescriptions :: [String]
levelDescriptions = ["Trivial", "Easy", "Medium", "Hard", "Hardcore", "Absurd"]

objectSymbol :: String
objectSymbol = "+"

drawScreen :: State -> IO ()
drawScreen state@(State _ _ move _) = do
        clearScreen
        evalSGR move
        drawScreen' state

drawScreen' :: State -> IO ()
drawScreen' state@(State topObject botObject move score)
        | move == Stop = drawGameOver state
        | otherwise = do
                drawStandard state
                drawScore score
    where
        (xTop, _) = topObject
        (xBot, _) = botObject

drawGameOver :: State -> IO ()
drawGameOver state@(State _ _ _ score) | score == 6 = drawWin
                                       | otherwise  = drawDefeat state

drawWin :: IO ()
drawWin = draw textPosition "You won the game my dude!"

drawDefeat :: State -> IO ()
drawDefeat state@(State topObject botObject _ score) = do
        drawConnection xTop xBot
        drawStandard state
        draw textPosition
                $  "Game Over - Score: "
                ++ show score
                ++ ".\n Press r to start over.\n Press q to quit."
    where
        (xTop, _) = topObject
        (xBot, _) = botObject


drawStandard :: State -> IO ()
drawStandard (State topObject botObject _ _) = do
        drawRow 0
        drawRow screenHeight
        drawLine (xTop, 0)            topObject
        drawLine (xBot, screenHeight) botObject
        draw (xTop, 0) objectSymbol
        draw (xBot, screenHeight) objectSymbol
        draw topObject objectSymbol
        draw botObject objectSymbol
    where
        (xTop, _) = topObject
        (xBot, _) = botObject

drawConnection :: Int -> Int -> IO ()
drawConnection xTop xBot = drawLine (xTop, middleHeight) (xBot, middleHeight)
    where
        middleHeight = screenHeight `div` 2
        min          = minimum values
        max          = maximum values
        values       = [xTop, xBot]

drawScore :: Int -> IO ()
drawScore score =
        draw textPosition
                $  "Match Me If You Can.\n Level: "
                ++ show score
                ++ " - "
                ++ (levelDescriptions !! score)
                ++ ".\n Press m to match."

evalSGR :: Move -> IO ()
evalSGR Idle = setSGR
        [SetConsoleIntensity NormalIntensity, SetColor Foreground Vivid White]
evalSGR Matching = setSGR
        [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Green]
evalSGR _ = setSGR
        [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red]

drawRow :: Int -> IO ()
drawRow y = drawRow' (screenWidth, y)
    where
        drawRow' (0, _) = return ()
        drawRow' (x, y) = do
                draw (x, y) ['-']
                drawRow' (x - 1, y)

drawLine :: Coord -> Coord -> IO ()
drawLine c1 c2 | y1 == y2  = drawHorizontalLine x1 x2 y1
               | x1 == x2  = drawVerticalLine y1 y2 x1
               | otherwise = return ()
    where
        (x1, y1) = c1
        (x2, y2) = c2

drawHorizontalLine :: Int -> Int -> Int -> IO ()
drawHorizontalLine x1 x2 y = drawHorizontalLine' min
    where
        drawHorizontalLine' x
                | x > max = return ()
                | otherwise = do
                        draw (x, y) "-"
                        drawHorizontalLine' (x + 1)
        min    = minimum values
        max    = maximum values
        values = [x1, x2]

drawVerticalLine :: Int -> Int -> Int -> IO ()
drawVerticalLine y1 y2 x = drawVerticalLine' min
    where
        drawVerticalLine' y
                | y > max = return ()
                | otherwise = do
                        draw (x, y) "|"
                        drawVerticalLine' (y + 1)
        min    = minimum values
        max    = maximum values
        values = [y1, y2]

draw :: Coord -> String -> IO ()
draw (x, y) symbol = do
        setCursorPosition y x
        putStr symbol

