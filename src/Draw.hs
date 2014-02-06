module Draw where

import Control.Lens
import System.Console.ANSI

import Game

drawBlock :: Int -> Int -> [String] -> IO ()
drawBlock y x = (mapM_ $ \(i, s) -> do
    setCursorPosition (y + i) x
    putStr s
    ) . zip [0..]

drawScreen :: Screen -> Game -> IO ()
drawScreen Start _ = do
    setCursorPosition 4 4
    putStr "Welcome to the Caves of Slight Danger"
    drawBlock 15 4 [ "Press [enter] to Start Playing"
                   , "Press [q] to Quit" ]

drawScreen Win _ = do
    setCursorPosition 4 4
    putStr "You WIN!"
    drawBlock 15 4 [ "Press [esc] to Quit"
                   , "Press <Anything> to Go back to Start" ]

drawScreen Lose _ = do
    setCursorPosition 4 4
    putStr "You LOSE!"
    drawBlock 15 4 [ "Press [esc] to Quit"
                   , "Press <Anything> to Go back to Start" ]

drawScreen Play game = do
    setCursorPosition 0 0
    drawLevel game

drawLevel :: Game -> IO ()
drawLevel game = do
    drawBlock 0 0 (map (\r -> (map (\t -> t^.glyph) r)) (game^.level))
    {-mapM_ (\r -> putStrLn (map (\t -> t^.glyph) r)) (game^.level)-}
    drawPlayer game

drawGame :: Game -> IO ()
drawGame game = drawScreen (ui game) game

resetColor :: IO ()
resetColor = setSGR [ SetColor Foreground Vivid Black ]

drawPlayer :: Game -> IO ()
drawPlayer game = do
    let (x,y) = game^.player^.location
        glyph = game^.player^.c_glyph
        color = game^.player^.c_color
    setSGR [ SetColor Foreground Vivid color ]
    setCursorPosition y x
    putStrLn [glyph]
