module Input where

import Prelude hiding (floor)

import Control.Lens
import Control.Monad (when)
import Control.Monad.State.Strict (State, modify, execState, get)
import Data.Array
import Data.Map.Strict as M (insert, delete)
import Data.Maybe (fromJust)

import Game
import World (creatureAt, tileAt, floor, wall)

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
         _ -> return ()


climb :: Climb -> GameState ()
climb = move . offsetClimb

attack :: Creature -> Creature -> GameState()
attack creature other = do
    let ap = creature^.attack_power
        other' = (hp -~ ap) other
    if other'^.hp < 1 then die other'
                      else updateCreature other'

die :: Creature -> GameState ()
die c = do
    game <- get
    let cs = delete (fromJust $ c^.c_id) (game^.creatures)
    creatures .= cs

updateCreature :: Creature -> GameState ()
updateCreature c = do
    game <- get
    let cs = insert (fromJust $ c^.c_id) c (game^.creatures)
    creatures .= cs
        
move :: Coord -> GameState ()
move offset = do
    game <- get
    let origin = game^.player.location
        move origin = origin <+> offset
        loc = move origin
    when (inBounds loc) $
        case creatureAt game loc of
             Just other -> attack (game^.player) other
             Nothing ->
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
