module Draw where

import Control.Lens
import Control.Monad (forM_)
import Data.Array as A
import Data.Map.Strict as M (elems, filter)
import System.Console.ANSI

import Game

drawGame :: Game -> IO ()
drawGame game = drawScreen (ui game) game

drawBlock :: Int -> Int -> [String] -> IO ()
drawBlock y x = (mapM_ $ \(i, s) -> do
    setCursorPosition (y + i) x
    putStr s
    ) . zip [0..]

drawStr :: Int -> Int -> String -> IO ()
drawStr y x str = setCursorPosition y x >> putStr str

drawScreen :: Screen -> Game -> IO ()
drawScreen Start _ = do
    drawStr 4 4 "Welcome to the Caves of Slight Danger"
    drawBlock 15 4 [ "Press [enter] to Start Playing"
                   , "Press [q] to Quit" ]

drawScreen Win _ = do
    drawStr 4 4 "You WIN!"
    drawBlock 15 4 [ "Press [esc] to Quit"
                   , "Press <Anything> to Go back to Start" ]

drawScreen Lose _ = do
    drawStr 4 4 "You LOSE!"
    drawBlock 15 4 [ "Press [esc] to Quit"
                   , "Press <Anything> to Go back to Start" ]

drawScreen Play game = do
    drawLevel game
    drawPlayer game
    drawCreatures game
    drawHud game

drawHud :: Game -> IO ()
drawHud game = do
    let p = game^.player
    resetColor
    drawStr gameHeight 0 $ "loc=" ++ show (p^.location) ++ " hp=[" ++ show (p^.hp) ++ "/" ++ show (p^.maxHp) ++ "]"

drawCreatures :: Game -> IO ()
drawCreatures game = forM_ (M.elems $ M.filter sameDepth $ game^.creatures) drawCreature
    where sameDepth c = depth == playerDepth
            where (_,_,depth) = c^.location
                  (_,_,playerDepth) = game^.player^.location

drawLevel :: Game -> IO ()
drawLevel game = do
    drawBlock 0 0 lvl
    where (_,_,depth) = game^.player.location
          lvl2d = (splitBy (gameWidth * gameHeight) $ A.elems $ game^.level) !! depth
          rows = splitBy gameWidth lvl2d
          lvl = map row2str rows
          row2str = foldr (\tile str -> (tile^.glyph) : str) ""

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
