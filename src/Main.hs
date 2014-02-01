{-# LANGUAGE TemplateHaskell #-}

module Main where

import System.Console.ANSI
import Control.Lens
import System.IO (hGetChar, hGetEcho, hSetEcho, stdin)

data Screen = Start | Win | Lose deriving (Show)

data Game = Game
    { _uis   :: [Screen]
    } deriving (Show)

makeLenses ''Game

drawScreen :: Screen -> Game -> IO ()
drawScreen Start _ = do
    setCursorPosition 4 4
    putStr "Welcome to the Caves of Slight Danger"
    setCursorPosition 15 4
    putStr "Press [enter] to Win"
    setCursorPosition 16 4
    putStr "Press [backspace] to Lose"
    setCursorPosition 17 4
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

drawGame :: Game -> IO ()
drawGame game = drawScreen (head (game ^. uis)) game

createGame :: Game
createGame = Game
    { _uis = [ Start ]
    }

getInput :: IO Char
getInput = hGetChar stdin

processInputScreen :: Screen -> Char -> Game -> Game
processInputScreen Start ch game =
    case ch of
         '\n' -> (uis .~ [Win]) game
         '\DEL' -> (uis .~ [Lose]) game
         'q' -> (uis .~ []) game
         _ -> game

processInputScreen Win ch game =
    case ch of
         '\ESC' -> (uis .~ []) game
         _ -> (uis .~ [Start]) game

processInputScreen Lose ch game =
    case ch of
         '\ESC' -> (uis .~ []) game
         _ -> (uis .~ [Start]) game

processInput :: Char -> Game -> Game
processInput ch game = processInputScreen (head (game ^. uis)) ch game

emptyUis :: Game -> Bool
emptyUis game = null (game ^. uis)

gameLoop :: Game -> IO ()
gameLoop game = do
    clearScreen
    drawGame game
    ch <- getInput
    let game' = processInput ch game
    if (emptyUis game')
       then return ()
       else gameLoop game'

main :: IO ()
main = do
    echo <- hGetEcho stdin
    hSetEcho stdin False
    gameLoop (createGame)
    setCursorPosition 0 0
    clearScreen
    hSetEcho stdin echo

drawHero :: IO ()
drawHero = do
    setSGR [ SetConsoleIntensity BoldIntensity
           , SetColor Foreground Vivid Blue ]
    putStrLn "@"
