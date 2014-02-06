module Draw where

import Control.Lens
import System.Console.ANSI

import Game

drawScreen :: Screen -> Game -> IO ()
drawScreen Start _ = do
    setCursorPosition 4 4
    putStr "Welcome to the Caves of Slight Danger"
    setCursorPosition 15 4
    putStr "Press [enter] to Start Playing"
    setCursorPosition 16 4
    putStr "Press [q] to Quit"

drawScreen Win _ = do
    setCursorPosition 4 4
    putStr "You WIN!"
    setCursorPosition 15 4
    putStr "Press [esc] to Quit"
    setCursorPosition 16 4
    putStr "Press <Anything> to Go back to Start"

drawScreen Lose _ = do
    setCursorPosition 4 4
    putStr "You LOSE!"
    setCursorPosition 15 4
    putStr "Press [esc] to Quit"
    setCursorPosition 16 4
    putStr "Press <Anything> to Go back to Start"

drawScreen Play game = do
    setCursorPosition 0 0
    drawLevel game

drawLevel :: Game -> IO ()
drawLevel game = do
    resetColor
    mapM_ (\r -> putStrLn (map (\t -> t^.glyph) r)) (game^.level)
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
    resetColor