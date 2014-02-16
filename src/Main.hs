{-# LANGUAGE TemplateHaskell #-}

module Main where

import Prelude hiding (floor)

import Control.Lens
import Control.Monad (forM_)
import Control.Monad.State.Strict (execState, get, execStateT)
import Data.Map.Strict as M (elems)
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

-- TODO This feels really ugly
styles :: [Style]
styles = [ CH.defaultStyle
         , AttributeStyle [Bold] DefaultF DarkBlueB  -- player
         , AttributeStyle [Bold] GreenF DefaultB     -- fungus
         , AttributeStyle [Bold] DarkBlueF DefaultB  -- out of sight tile
         ]

emptyUis :: Game -> Bool
emptyUis game = null $ game^.uis

tick :: GameState ()
tick = do
    game <- get
    forM_ (M.elems $ game^.creatures) creatureTick
    updateVisibleTiles

gameLoop :: Game -> IO ()
gameLoop game = do
    erase
    game' <- execStateT drawGame game
    ch <- getInput
    let game'' = (execState $ processInput ch >> tick) game'
    if emptyUis game''
       then return ()
       else gameLoop game''

main :: IO ()
main = do
    initCurses
    startColor
    win <- initScr
    cstyles <- convertStyles Main.styles
    echo False
    cursSet CursorInvisible
    createGame win cstyles >>= gameLoop
    move 0 0
    erase
    echo True
    cursSet CursorVisible
    endWin

