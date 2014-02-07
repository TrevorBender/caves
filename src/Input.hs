module Input where

import Prelude hiding (floor)

import Control.Lens
import Control.Monad (when)
import Control.Monad.State (State, modify, execState, get)
import Data.Array

import Game
import World

getInput :: IO Char
getInput = getChar

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
         '>' -> climb Down
         '<' -> climb Up
         's' -> smoothGame
         _ -> return ()


climb :: Climb -> GameState ()
climb = move . offsetClimb
        
move :: Coord -> GameState ()
move offset = do
    game <- get
    let origin = game^.player.location
        move origin = origin <+> offset
        loc = move origin
    when (inBounds loc) $
        if tileAt game loc == floor
           then (player.location) .= loc
           else dig loc
    return ()

movePlayer :: Direction -> GameState ()
movePlayer = move . offsetDir

dig :: Coord -> GameState ()
dig loc = do
    game <- get
    let lvl = game^.level
        lvl' = lvl//[(reverseCoord loc, floor)]
    level .= lvl'

processInput :: Char -> GameState ()
processInput ch = do
    game <- get
    processInputScreen (ui game) ch
