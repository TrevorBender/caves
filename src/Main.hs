{-# LANGUAGE TemplateHaskell #-}

module Main where

import Prelude hiding (floor)

import Control.Lens
import Control.Monad (forM_)
import Control.Monad.State.Strict (execState, get)
import Data.Map.Strict as M (elems)
import System.Console.ANSI
import System.IO (hGetEcho, hSetEcho, stdin)

import Draw (drawGame, resetColor)
import Game
import Input (getInput, processInput)
import Generation (createGame)
import Creature (creatureTick)

emptyUis :: Game -> Bool
emptyUis game = null $ game^.uis

tick :: GameState ()
tick = do
    game <- get
    forM_ (M.elems $ game^.creatures) creatureTick

gameLoop :: Game -> IO ()
gameLoop game = do
    clearScreen
    resetColor
    drawGame game
    ch <- getInput
    let game' = (execState $ processInput ch >> tick) game
    if emptyUis game'
       then return ()
       else gameLoop game'

main :: IO ()
main = do
    echo <- hGetEcho stdin
    hSetEcho stdin False
    hideCursor
    createGame >>= gameLoop
    setCursorPosition 0 0
    clearScreen
    showCursor
    hSetEcho stdin echo

