module Main where

import Prelude hiding (floor)

import Control.Lens
import Control.Monad (forM_, when, unless)
import Control.Monad.State.Strict (execState, get, execStateT)
import Data.Map.Strict as M (Map, elems, fromAscList, keys)
import System.IO (hGetEcho, hSetEcho, stdin)
import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper as CH

import Draw (drawGame, resetColor)
import Game
import Input (getInput, processInput)
import Generation (createGame)
import Creature (creatureTick)
import World (updateVisibleTiles)

import Line

emptyUis :: Game -> Bool
emptyUis game = null $ game^.uis

tick :: GameState ()
tick = do
    cs <- use $ creatures .to M.elems
    forM_ cs creatureTick
    updateVisibleTiles

nehw = flip when

gameLoop :: Game -> IO ()
gameLoop game = do
    erase
    game' <- execStateT drawGame game
    ch <- getInput
    let game'' = (execState $ processInput ch >> gameChanged >>= nehw tick) game'
    unless (emptyUis game'') $ gameLoop game''

main :: IO ()
main = do
    initCurses
    startColor
    win <- initScr
    cstyles <- convertStyles $ elems styleMap
    echo False
    cursSet CursorInvisible
    let cstyleMap = M.fromAscList $ zip (keys styleMap) cstyles
    createGame win cstyleMap >>= gameLoop
    move 0 0
    erase
    echo True
    cursSet CursorVisible
    endWin

