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
         'h' -> movePlayer W  game 
         'l' -> movePlayer E  game 
         'k' -> movePlayer N  game 
         'j' -> movePlayer S  game 
         'y' -> movePlayer NW game 
         'u' -> movePlayer NE game 
         'b' -> movePlayer SW game 
         'n' -> movePlayer SE game 
         _ -> game

data Direction = N | E | S | W
               | NE | SE | SW | NW

offsetDir :: Direction -> Coord
offsetDir N = (0, -1)
offsetDir E = (1, 0)
offsetDir S = (0, 1)
offsetDir W = (-1, 0)
offsetDir NE = (offsetDir N) <+> (offsetDir E)
offsetDir SE = (offsetDir S) <+> (offsetDir E)
offsetDir SW = (offsetDir S) <+> (offsetDir W)
offsetDir NW = (offsetDir N) <+> (offsetDir W)

(<+>) :: Coord -> Coord -> Coord
(x,y) <+> (x',y') = (x + x',y + y')

movePlayer :: Direction -> Game -> Game
movePlayer dir game = ((player.location) .~ (loc <+> (offsetDir dir))) game
    where loc = game^.player^.location

processInput :: Char -> Game -> Game
processInput ch game = processInputScreen (ui game) ch game
