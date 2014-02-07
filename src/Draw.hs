module Draw where

import Control.Lens
import Control.Monad (forM_)
import Data.Array
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
    drawBlock 0 0 lvl
    drawPlayer game
    forM_ (game^.creatures) drawCreature
    where (_,_,depth) = game^.player.location
          lvl2d = (splitBy (gameWidth * gameHeight) $ elems $ game^.level) !! depth
          rows = splitBy gameWidth lvl2d
          lvl = map row2str rows
          row2str = foldr (\tile str -> (tile^.glyph) : str) ""

drawGame :: Game -> IO ()
drawGame game = drawScreen (ui game) game

resetColor :: IO ()
resetColor = setSGR [ SetColor Foreground Vivid Black ]

drawCreature :: Creature -> IO ()
drawCreature creature = do
    let (x,y,_) = creature^.location
        glyph = creature^.c_glyph
        color = creature^.c_color
    setSGR [ SetColor Foreground Vivid color ]
    setCursorPosition y x
    putStrLn [glyph]

drawPlayer :: Game -> IO ()
drawPlayer game = drawCreature (game^.player)
