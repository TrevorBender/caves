module Input where

import Prelude hiding (floor)

import Control.Lens
import Control.Monad (when)
import Control.Monad.State.Strict (get)
import Data.Array
import UI.HSCurses.Curses as C

import Game
import Creature (attack)
import World (creatureAt, isFloor, floor)

getInput :: IO Key
getInput = getCh

processInputScreen :: Screen -> Key -> GameState ()
processInputScreen Start key =
    case key of
         KeyChar '\n' -> uis .= [Play]
         KeyChar 'q' -> uis .= []
         _ -> return ()

processInputScreen Win key =
    case key of
         KeyChar '\ESC' -> uis .= []
         _ -> uis .= [Start]

processInputScreen Lose key =
    case key of
         KeyChar '\ESC' -> uis .= []
         _ -> uis .= [Start]

processInputScreen Play key =
    case key of
         KeyChar '\n' -> uis .= [Win]
         KeyChar '\DEL' -> uis .= [Lose]
         KeyChar 'q' -> uis .= []
         KeyChar 'h' -> movePlayer W
         KeyChar 'l' -> movePlayer E
         KeyChar 'k' -> movePlayer N
         KeyChar 'j' -> movePlayer S
         KeyChar 'y' -> movePlayer NW
         KeyChar 'u' -> movePlayer NE
         KeyChar 'b' -> movePlayer SW
         KeyChar 'n' -> movePlayer SE
         KeyChar '>' -> climb Down
         KeyChar '<' -> climb Up
         _ -> return ()

climb :: Climb -> GameState ()
climb = Input.move . offsetClimb

move :: Coord -> GameState ()
move offset = do
    game <- get
    let origin = game^.player.location
        move origin = origin <+> offset
        loc = move origin
    when (inBounds loc) $ do
        mc <- creatureAt loc
        canMove <- isFloor loc
        case mc of
             Just other -> attack (game^.player) other
             Nothing ->
                 if canMove
                    then (player.location) .= loc
                    else dig loc
    return ()

movePlayer :: Direction -> GameState ()
movePlayer = Input.move . offsetDir

dig :: Coord -> GameState ()
dig loc = do
    game <- get
    let lvl = game^.world
        lvl' = lvl//[(reverseCoord loc, floor)]
    world .= lvl'

processInput :: Key -> GameState ()
processInput key = do
    game <- get
    processInputScreen (ui game) key
