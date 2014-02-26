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
import Creature (move, playerPickup, playerDropItem, equip, eat)
import World (creatureAt, isFloor, floor, tileAt', stairsDown, stairsUp)

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
         'x' -> pushScreen EquipItem
         'e' -> pushScreen EatItem
         _   -> updated .= False

processInputScreen DropItem key = inventoryScreen key $ \ix -> playerDropItem ix

processInputScreen EquipItem key = inventoryScreen key $ \ix -> do
    item <- use $ player.inventory. to (!! ix)
    equip item

processInputScreen EatItem key = inventoryScreen key $ \ix -> do
    item <- use $ player.inventory .to (!! ix)
    eat item

inventoryScreen :: Char -> (Int -> GameState()) -> GameState ()
inventoryScreen key action = do
    mix <- selectedItem key
    when (isJust mix) $ do
        let ix = fromJust mix
        action ix
        updated .= True
        dropScreen
    case key of
         '\ESC' -> dropScreen
         _ -> return ()

selectedItem :: Char -> GameState (Maybe Int)
selectedItem key = do
    inv <- use $ player.inventory
    let ks = take (length $ inv) $ zip [0..] ['a'..]
        ks' = filter (\(_,k) -> k == key) ks
    return $ if null ks' then Nothing else Just $ (fst . head) ks'

endGame :: GameState ()
endGame = do
    inv <- use $ player.inventory
    let vi = filter (\i -> i^.i_glyph == '*') inv
    when (null vi) lose
    when (not $ null vi) win

climb :: Climb -> GameState ()
climb dir = do
    p <- use player
    let loc@(_,_,depth) = p^.location
    tile <- tileAt' loc
    when (tile == stairsDown && dir == Down) $
        move p $ offsetClimb dir
    when (tile == stairsUp && dir == Up) $ do
        when (depth == 0) $
            endGame
        when (depth /= 0) $
            move p $ offsetClimb dir

movePlayer :: Direction -> GameState ()
movePlayer dir = do
    p <- use player
    move p $ offsetDir dir

processInput :: Key -> GameState ()
processInput (KeyChar key) = do
    updated .= True
    screen <- ui
    when (key == 'q') quit
    processInputScreen screen key
