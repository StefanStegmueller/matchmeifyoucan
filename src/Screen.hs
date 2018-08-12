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
textPosition = (1, 10)

levelDescriptions :: [String]
levelDescriptions = ["Trivial", "Easy", "Medium", "Hard", "Hardcore", "Absurd"]

drawScreen :: State -> IO ()
drawScreen state@(State _ _ move _) = do
        clearScreen
        evalSGR move
        drawScreen' state

drawScreen' :: State -> IO ()
drawScreen' (State topObject botObject move score)
        | move == Stop = drawGameOver score
        | otherwise = do
                drawLine 0
                drawLine screenHeight
                draw topObject ['+']
                draw botObject ['+']
                drawScore score

drawGameOver :: Int -> IO ()
drawGameOver score | score == 6 = drawWin
                   | otherwise  = drawDefeat score

drawWin :: IO ()
drawWin = draw textPosition "You won the game my dude!"

drawDefeat :: Int -> IO ()
drawDefeat score =
        draw textPosition
                $  "Game Over - Score: "
                ++ show score
                ++ ".\n Press r to start over.\n Press q to quit."

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

