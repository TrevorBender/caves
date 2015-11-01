module Main where

import Prelude hiding (floor)

import Control.Lens
import Control.Monad (forM_, when, unless)
import Control.Monad.State.Strict (execState, get, execStateT)
import Data.Map.Strict as M (Map, elems, fromAscList, keys)
import System.IO (hGetEcho, hSetEcho, stdin)
import UI.HSCurses.Curses
    ( erase
    , initCurses
    , initScr
    , echo
    , cursSet
    , CursorVisibility(..)
    , move
    , startColor
    , endWin
    )
import UI.HSCurses.CursesHelper as CH ( convertStyles
                                      , defaultStyle
                                      , Style(..)
                                      , ForegroundColor(..)
                                      , BackgroundColor(..)
                                      , Attribute(..)
                                      )

import Draw (drawGame, resetColor)
import Game ( gameChanged )
import Types
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

styleMap :: Map StyleType Style
styleMap = M.fromAscList
         [ (DefaultStyle, defaultStyle)
         , (PlayerStyle, AttributeStyle [Bold] DefaultF DarkBlueB)
         , (FungusStyle, AttributeStyle [Bold] GreenF DefaultB)
         , (OutOfSiteStyle, AttributeStyle [Bold] WhiteF BlackB)
         , (BatStyle, AttributeStyle [Bold] BrownF DefaultB)
         , (VictoryItemStyle, AttributeStyle [Bold] YellowF DefaultB)
         , (SwordStyle, AttributeStyle [] CyanF DefaultB)
         , (StaffStyle, AttributeStyle [] BrownF DefaultB)
         , (ZombieStyle, AttributeStyle [Bold] RedF DefaultB)
         ]


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

