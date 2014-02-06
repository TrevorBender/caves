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
         'h' -> movePlayer game W
         'l' -> movePlayer game E
         'k' -> movePlayer game N
         'j' -> movePlayer game S
         _ -> game

data Direction = N | E | S | W

offsetDir :: Direction -> Coord
offsetDir N = (0, -1)
offsetDir E = (1, 0)
offsetDir S = (0, 1)
offsetDir W = (-1, 0)

(<+>) :: Coord -> Coord -> Coord
(x,y) <+> (x',y') = (x + x',y + y')

movePlayer :: Game -> Direction -> Game
movePlayer game dir = ((player.location) .~ (loc <+> (offsetDir dir))) game
    where loc = game^.player^.location

processInput :: Char -> Game -> Game
processInput ch game = processInputScreen (ui game) ch game
