module Input where

import Control.Lens

import Game

getInput :: IO Char
getInput = getChar

processInputScreen :: Screen -> Char -> Game -> Game
processInputScreen Start ch game =
    case ch of
         '\n' -> (uis .~ [Play]) game
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

processInputScreen Play ch game =
    case ch of
         '\n' -> (uis .~ [Win]) game
         '\DEL' -> (uis .~ [Lose]) game
         'q' -> (uis .~ []) game
         _ -> game

processInput :: Char -> Game -> Game
processInput ch game = processInputScreen (ui game) ch game
