module Input
    ( processInput
    , getInput
    ) where

import Prelude hiding (floor)

import Control.Lens
import Control.Monad (when)
import Control.Monad.State.Strict (get)
import Data.Array
import Data.Map.Strict as M (adjust)
import UI.HSCurses.Curses as C (Key(..), getCh)

import Game
import Creature (move)
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
        move (game^.player) $ offsetClimb dir
    when (tile == stairsUp && dir == Up) $ do
        when (depth == 0) $
            win
        when (depth /= 0) $
            move (game^.player) $ offsetClimb dir

movePlayer :: Direction -> GameState ()
movePlayer dir = do
    game <- get
    let creature = game^.player
    move creature $ offsetDir dir

processInput :: Key -> GameState ()
processInput key = do
    game <- get
    processInputScreen (ui game) key
