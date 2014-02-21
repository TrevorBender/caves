{-# LANGUAGE TemplateHaskell #-}

module Main where

import Prelude hiding (floor)

import Control.Lens
import Control.Monad (forM_)
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

styleMap :: Map StyleType Style
styleMap = M.fromAscList
         [ (DefaultStyle, CH.defaultStyle)
         , (PlayerStyle, AttributeStyle [Bold] DefaultF DarkBlueB)
         , (FungusStyle, AttributeStyle [Bold] GreenF DefaultB)
         , (OutOfSiteStyle, AttributeStyle [Bold] DarkBlueF DefaultB)
         , (BatStyle, AttributeStyle [Bold] BrownF DefaultB)
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

