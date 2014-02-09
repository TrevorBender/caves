module Draw where

import Control.Lens
import Control.Monad (forM_)
import Data.Array as A
import Data.Map.Strict as M (elems, filter)
import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper

import Game

drawGame :: Game -> IO ()
drawGame game = do
    drawScreen (ui game) game
    refresh

drawBlock :: Game -> Int -> Int -> [String] -> IO ()
drawBlock game y x = (mapM_ $ \(i, s) -> do
    mvWAddStr (game^.window) (y + i) x s
    ) . zip [0..]

drawStr :: Game -> Int -> Int -> String -> IO ()
drawStr game y x str = mvWAddStr (game^.window) y x str

drawScreen :: Screen -> Game -> IO ()
drawScreen Start game = do
    drawStr game 4 4 "Welcome to the Caves of Slight Danger"
    drawBlock game 15 4 [ "Press [enter] to Start Playing"
                        , "Press [q] to Quit" ]

drawScreen Win game = do
    drawStr game 4 4 "You WIN!"
    drawBlock game 15 4 [ "Press [esc] to Quit"
                        , "Press <Anything> to Go back to Start" ]

drawScreen Lose game = do
    drawStr game 4 4 "You LOSE!"
    drawBlock game 15 4 [ "Press [esc] to Quit"
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
    drawStr game gameHeight 0 $
        "loc=" ++ show (p^.location) ++ " hp=[" ++ show (p^.hp) ++ "/" ++ show (p^.maxHp) ++ "]"

drawCreatures :: Game -> IO ()
drawCreatures game = forM_ (M.elems $ M.filter sameDepth $ game^.creatures) (drawCreature game)
    where sameDepth c = depth == playerDepth
            where (_,_,depth) = c^.location
                  (_,_,playerDepth) = game^.player^.location

drawLevel :: Game -> IO ()
drawLevel game = do
    drawBlock game 0 0 lvl
    where (_,_,depth) = game^.player.location
          lvl2d = (splitBy (gameWidth * gameHeight) $ A.elems $ game^.world) !! depth
          rows = splitBy gameWidth lvl2d
          lvl = map row2str rows
          row2str = foldr (\tile str -> (tile^.glyph) : str) ""

resetColor :: IO ()
resetColor = resetStyle

drawCreature :: Game -> Creature -> IO ()
drawCreature game creature = do
    let (x,y,_) = creature^.location
        glyph = creature^.c_glyph
        style = creature^.c_style
        cstyle = nthStyle style game
    setStyle cstyle
    drawStr game y x [glyph]

drawPlayer :: Game -> IO ()
drawPlayer game = drawCreature game (game^.player)
