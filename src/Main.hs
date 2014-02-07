{-# LANGUAGE TemplateHaskell #-}

module Main where

import Prelude hiding (floor)

import Control.Lens
import Control.Monad (replicateM_)
import Control.Monad.State.Strict (execState)
import System.Console.ANSI
import System.IO (hGetChar, hGetEcho, hSetEcho, stdin)
import System.Random (getStdGen, randomR, randomRs, RandomGen(..))

import Draw
import Game
import Input
import World

emptyUis :: Game -> Bool
emptyUis game = null $ game^.uis

createPlayer :: Coord -> Creature
createPlayer loc = Creature
    { _location = loc
    , _c_glyph = '@'
    , _c_color = Blue }

createGame :: IO Game
createGame = do
    level <- createLevel
    let world = (execState $ replicateM_ 8 smoothWorld) level
    g <- getStdGen
    let thePlayer = createPlayer (0, 0, 0)
        game = Game { _uis = [ Start ]
                    , _level = world
                    , _player = thePlayer
                    }
        playerLoc = findEmptyLocation g 0 game
    return $ (player.location .~ playerLoc) game

gameLoop :: Game -> IO ()
gameLoop game = do
    clearScreen
    resetColor
    drawGame game
    ch <- getInput
    let game' = (execState $ processInput ch) game
    if (emptyUis game')
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

