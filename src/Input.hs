module Input
    ( processInput
    , getInput
    ) where

import Prelude hiding (floor)

import Control.Lens
import Control.Monad (when)
import Control.Monad.State.Strict (get, execState)
import Data.Array
import Data.Map.Strict as M (adjust)
import Data.Maybe (isJust, fromJust)
import UI.HSCurses.Curses as C (Key(..), getCh)

import Game
import Creature (move, playerPickup, playerDropItem, equip, eat, levelUpActions)
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
         'x' -> pushScreen ExamineItem
         'w' -> pushScreen EquipItem
         'e' -> pushScreen EatItem
         '?' -> pushScreen Help >> updated .= False
         _   -> updated .= False

processInputScreen DropItem key = inventoryScreen key dropItemFilter $ \item -> playerDropItem item

processInputScreen EquipItem key = inventoryScreen key equipItemFilter $ \item -> equip item

processInputScreen EatItem key = inventoryScreen key eatItemFilter $ \item -> eat item

processInputScreen ChooseLevelUp key =
    case lookup key levelUpActions of
         Just a -> player %= execState a >> dropScreen
         _ -> return ()

processInputScreen Help _ = dropScreen >> updated .= False

processInputScreen ExamineItem key = inventoryScreen key examineItemFilter $ \item -> do
    updated .= False
    pl <- use $ player.location
    notify pl $ itemStr item
    where itemStr item = "It's a " ++ item^.i_name ++ ". " ++ itemDetails item
          itemDetails item = let showIfValue i s = if i /= 0 then s ++ show i else ""
                                 a = showIfValue (item^.i_attackPower) " attack: "
                                 d = showIfValue (item^.i_defensePower) " defense: "
                                 f = showIfValue (item^.i_foodValue) " food: "
                                 in a ++ d ++ f

inventoryScreen :: Char -> (Item -> Bool) -> (Item -> GameState()) -> GameState ()
inventoryScreen key filt action = do
    mi <- selectedItem key filt
    when (isJust mi) $ do
        let i = fromJust mi
        updated .= True
        action i
        dropScreen
    case key of
         '\ESC' -> dropScreen
         _ -> return ()

selectedItem :: Char -> (Item -> Bool) -> GameState (Maybe Item)
selectedItem key filt = do
    inv <- use $ player.inventory
    let ks = filter (\(_,i) -> filt i) . zip ['a'..] $ inv
    return $ lookup key ks

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
