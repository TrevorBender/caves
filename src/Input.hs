module Input
    ( processInput
    , getInput
    ) where

import Prelude hiding (floor)

import Control.Lens
import Control.Monad (when)
import Control.Monad.State.Strict (get)
import Data.Array
import UI.HSCurses.Curses as C

import Game
import Creature (attack)
import World (creatureAt, isFloor, floor, tileAt, stairsDown, stairsUp)

getInput :: IO Key
getInput = getCh

processInputScreen :: Screen -> Key -> GameState ()
processInputScreen Start key =
    case key of
         KeyChar '\n' -> uis .= [Play]
         KeyChar 'q' -> quit
         _ -> return ()

processInputScreen Win key =
    case key of
         KeyChar '\ESC' -> quit
         _ -> uis .= [Start]

processInputScreen Lose key =
    case key of
         KeyChar '\ESC' -> quit
         _ -> uis .= [Start]

processInputScreen Play key =
    case key of
         KeyChar '\DEL' -> lose
         KeyChar 'q' -> quit
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
         KeyChar 'r' -> drawRegions %= not
         _ -> return ()

climb :: Climb -> GameState ()
climb dir = do
    game <- get
    let loc@(_,_,depth) = game^.player^.location
        tile = tileAt game loc
    when (tile == stairsDown && dir == Down) $
        Input.move $ offsetClimb dir
    when (tile == stairsUp && dir == Up) $ do
        when (depth == 0) $
            win
        when (depth /= 0) $
            Input.move $ offsetClimb dir

canMove :: Coord -> GameState Bool
canMove loc = do
    game <- get
    let tile = tileAt game loc
    return $ tile^.kind `elem` [ Floor, StairsUp, StairsDown ]

move :: Coord -> GameState ()
move offset = do
    game <- get
    let origin = game^.player.location
        move origin = origin <+> offset
        loc = move origin
    when (inBounds loc) $ do
        mc <- creatureAt loc
        canMove <- canMove loc
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
    notify $ "You dig."

processInput :: Key -> GameState ()
processInput key = do
    game <- get
    processInputScreen (ui game) key
