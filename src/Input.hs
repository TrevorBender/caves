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
processInputScreen Start (KeyChar key) =
    case key of
         '\n' -> uis .= [Play]
         'q' -> quit
         _ -> return ()

processInputScreen Win (KeyChar key) =
    case key of
         '\ESC' -> quit
         _ -> uis .= [Start]

processInputScreen Lose (KeyChar key) =
    case key of
         '\ESC' -> quit
         _ -> uis .= [Start]

processInputScreen Play (KeyChar key) =
    case key of
         '\DEL' -> lose
         'q' -> quit
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

processInputScreen _ _ = return ()

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
