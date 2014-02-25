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
import Data.Maybe (isJust, fromJust)
import UI.HSCurses.Curses as C (Key(..), getCh)

import Game
import Creature (move, playerPickup, playerDropItem, equip)
import World (creatureAt, isFloor, floor, tileAt, stairsDown, stairsUp)

getInput :: IO Key
getInput = getCh

processInputScreen :: Screen -> Char -> GameState ()
processInputScreen Start key =
    case key of
         _ -> uis .= [Play]

processInputScreen Win key =
    case key of
         _ -> quit

processInputScreen Lose key =
    case key of
         _ -> quit

processInputScreen Play key =
    case key of
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
         ',' -> playerPickup
         'd' -> pushScreen DropItem
         'e' -> pushScreen EquipItem
         _ -> updated .= False

processInputScreen DropItem key = inventoryScreen key $ \ix -> playerDropItem ix

processInputScreen EquipItem key = inventoryScreen key $ \ix -> do
    game <- get
    let inv = game^.player.inventory
        item = inv !! ix
    equip item

inventoryScreen :: Char -> (Int -> GameState()) -> GameState ()
inventoryScreen key action = do
    mix <- selectedItem key
    when (isJust mix) $ do
        let ix = fromJust mix
        action ix
        updated .= True
        dropScreen
    case key of
         '\ESC' -> uis %= init
         _ -> return ()

selectedItem :: Char -> GameState (Maybe Int)
selectedItem key = do
    game <- get
    let ks = take (length $ game^.player.inventory) $ zip [0..] ['a'..]
        ks' = filter (\(_,k) -> k == key) ks
    return $ if null ks' then Nothing else Just $ (fst . head) ks'

endGame :: GameState ()
endGame = do
    game <- get
    let inv = game^.player.inventory
        vi = filter (\i -> i^.i_glyph == '*') inv
    when (null vi) lose
    when (not $ null vi) win

climb :: Climb -> GameState ()
climb dir = do
    game <- get
    let loc@(_,_,depth) = game^.player^.location
        tile = tileAt game loc
    when (tile == stairsDown && dir == Down) $
        move (game^.player) $ offsetClimb dir
    when (tile == stairsUp && dir == Up) $ do
        when (depth == 0) $
            endGame
        when (depth /= 0) $
            move (game^.player) $ offsetClimb dir

movePlayer :: Direction -> GameState ()
movePlayer dir = do
    game <- get
    let creature = game^.player
    move creature $ offsetDir dir

processInput :: Key -> GameState ()
processInput (KeyChar key) = do
    game <- get
    updated .= True
    when (key == 'q') quit
    processInputScreen (ui game) key
