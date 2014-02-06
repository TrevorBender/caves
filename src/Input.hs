module Input where

import Control.Lens
import Control.Monad.State (State, modify, execState)

import Game

getInput :: IO Char
getInput = getChar

type GameState = State Game

processInputScreen :: Screen -> Char -> GameState ()
processInputScreen Start ch =
    case ch of
         '\n' -> modify (uis .~ [Play])
         'q' -> modify (uis .~ [])
         _ -> return ()

processInputScreen Win ch =
    case ch of
         '\ESC' -> modify (uis .~ [])
         _ -> modify (uis .~ [Start])

processInputScreen Lose ch =
    case ch of
         '\ESC' -> modify (uis .~ [])
         _ -> modify (uis .~ [Start])

processInputScreen Play ch =
    case ch of
         '\n' -> uis .= [Win]
         '\DEL' -> uis .= [Lose]
         'q' -> uis .= []
         'h' -> movePlayer W
         'l' -> movePlayer E
         'k' -> movePlayer N
         'j' -> movePlayer S
         'y' -> movePlayer NW
         'u' -> movePlayer NE
         'b' -> movePlayer SW
         'n' -> movePlayer SE
         _ -> return ()

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

movePlayer :: Direction -> GameState ()
movePlayer dir = (player.location) %= move
    where move origin = origin <+> (offsetDir dir)

processInput :: Char -> Game -> Game
processInput ch game = (execState $ processInputScreen (ui game) ch) game
