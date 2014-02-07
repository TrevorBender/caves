module Input where

import Prelude hiding (floor)

import Control.Lens
import Control.Monad.State (State, modify, execState, get)
import Data.Map.Strict as M

import Game
import World

getInput :: IO Char
getInput = getChar

type GameState = State Game

processInputScreen :: Screen -> Char -> GameState ()
processInputScreen Start ch =
    case ch of
         '\n' -> uis .= [Play]
         'q' -> uis .= []
         _ -> return ()

processInputScreen Win ch =
    case ch of
         '\ESC' -> uis .= []
         _ -> uis .= [Start]

processInputScreen Lose ch =
    case ch of
         '\ESC' -> uis .= []
         _ -> uis .= [Start]

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
movePlayer dir = do
    game <- get
    let origin = game^.player.location
        loc = move origin
    if tileAt game loc == floor
       then (player.location) .= loc
       else dig loc
    where move origin = origin <+> (offsetDir dir)

dig :: Coord -> GameState ()
dig (x,y) = do
    game <- get
    let lvl = game^.level
        row = lvl ! y
        row' = M.adjust (\_ -> floor) x row
        lvl' = M.adjust (\_ -> row') y lvl
    level .= lvl'


processInput :: Char -> GameState ()
processInput ch = do
    game <- get
    processInputScreen (ui game) ch
