module Screen
        ( drawScreen
        , screenWidth
        , screenHeight
        )
where

import           System.Console.ANSI
import           Mechanics
import           Internal

scorePosition :: Coord
scorePosition = (1, 10)

levelDescriptions :: [String]
levelDescriptions = ["Trivial", "Easy", "Medium", "Hard", "Hardcore", "Absurd"]

drawScreen :: State -> IO ()
drawScreen (State topObject botObject state score) = do
        clearScreen
        evalSGR state
        drawLine 0
        drawLine screenHeight
        draw topObject ['+']
        draw botObject ['+']
        drawScore score

drawScore :: Int -> IO ()
drawScore score =
        draw scorePosition
                $  "Level: "
                ++ show score
                ++ " - "
                ++ (levelDescriptions !! score)

evalSGR :: Move -> IO ()
evalSGR Idle = setSGR
        [SetConsoleIntensity NormalIntensity, SetColor Foreground Vivid White]
evalSGR Matching = setSGR
        [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red]

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

