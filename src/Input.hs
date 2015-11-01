module Input
    ( processInput
    , getInput
    ) where

import Prelude hiding (floor)

import Control.Lens
import Control.Monad (when, unless)
import Control.Monad.State.Strict (get, execState)
import Data.Array
import Data.Foldable (forM_)
import Data.List as L (delete)
import Data.Map.Strict as M (insert)
import Data.Maybe (isJust, fromJust)
import UI.HSCurses.Curses as C (Key(..), getCh)

import Game
import Creature (move, playerPickup, playerDropItem, playerEquip, eat, levelUpActions, playerThrowAttack, playerRangedAttack, quaff, cast)
import World (creatureAt, isFloor, floor, tileAt', stairsDown, stairsUp, describe, describeItem, canSee', isCreature)

getInput :: IO Key
getInput = getCh

processInputScreen :: Screen -> Char -> GameState ()
processInputScreen Start key = uis .= [Play]

processInputScreen Win key = quit

processInputScreen Lose key = updated .= False

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
         'd' -> pushScreen DropItem >> updated .= False
         'x' -> pushScreen ExamineItem >> updated .= False
         'w' -> pushScreen EquipItem >> updated .= False
         'e' -> pushScreen EatItem >> updated .= False
         'i' -> pushScreen QuaffItem >> updated .= False
         'r' -> pushScreen CastSpell >> updated .= False
         't' -> pushScreen Throw >> updated .= False
         'g' -> pushScreen Look >> updated .= False
         'f' -> fireWeaponScreen
         '?' -> pushScreen Help >> updated .= False
         _   -> updated .= False

processInputScreen DropItem key = inventoryScreen key dropItemFilter $ \item -> playerDropItem item

processInputScreen EquipItem key = inventoryScreen key equipItemFilter $ \item -> playerEquip item

processInputScreen EatItem key = inventoryScreen key eatItemFilter $ \item -> eat item

processInputScreen QuaffItem key = inventoryScreen key quaffItemFilter $ \item -> quaff item

processInputScreen ChooseLevelUp key =
    case lookup key levelUpActions of
         Just a -> player %= execState a >> dropScreen
         _ -> return ()

processInputScreen Help _ = dropScreen >> updated .= False

processInputScreen ExamineItem key = inventoryScreen key examineItemFilter $ \item -> do
    updated .= False
    pl <- use $ player.location
    notify pl $ describeItem item

processInputScreen Look key = targetScreen key $ do
    pl <- use $ player.location
    tl <- use targetLoc
    describe tl >>= notify pl

processInputScreen ThrowItem key = inventoryScreen key throwItemFilter $ \item ->
    whenPlayerCanSee $ \p tl -> do
        isC <- isCreature tl
        if isC then do
            Just c <- creatureAt tl
            playerThrowAttack item c
            player.inventory %= delete item
        else do
            player.inventory %= delete item
            items %= insert tl (item { _itemLocation = tl })

processInputScreen Throw key = targetScreen key $ do
    pushScreen ThrowItem
    updated .= False

processInputScreen FireWeapon key = targetScreen key $ do
    updated .= True
    whenPlayerCanSee $ \p tl -> do
        isC <- isCreature tl
        when isC $ do
            Just c <- creatureAt tl
            playerRangedAttack c

processInputScreen CastSpell key = targetScreen key $ do
    pushScreen ReadItem
    updated .= False
processInputScreen ReadItem key = inventoryScreen key readItemFilter $ \item -> do
    targetItem .= Just item
    pushScreen ReadSpellBook
    updated .= False
processInputScreen ReadSpellBook key = do
    mti <- use targetItem
    case mti of
         Nothing -> dropScreen
         Just ti -> dropScreen >> readSpellBook key (ti^.itemSpells)

readSpellBook :: Char -> [Spell] -> GameState ()
readSpellBook key sps = do
    let lm = zip ['a'..] sps
        ms = lookup key lm
    forM_ ms cast

whenPlayerCanSee :: (Creature -> Coord -> GameState ()) -> GameState ()
whenPlayerCanSee action = do
    tl <- use targetLoc
    p <- use player
    isVis <- canSee' tl p
    when isVis $ action p tl

fireWeaponScreen :: GameState ()
fireWeaponScreen = do
    updated .= False
    mw <- use $ player.weapon
    when (isJust mw) $ do
        let Just w = mw
            ra = w^.iRangedAttackPower
        when (ra /= 0) $ pushScreen FireWeapon

targetScreen :: Char -> GameState () -> GameState ()
targetScreen key targetAction = do
    updated .= False
    case key of
         'k' -> moveCursor N
         'l' -> moveCursor E
         'j' -> moveCursor S
         'h' -> moveCursor W
         'u' -> moveCursor NE
         'n' -> moveCursor SE
         'b' -> moveCursor SW
         'y' -> moveCursor NW
         '\ESC' -> dropScreen
         '\n' -> dropScreen >> targetAction
         _      -> return ()
    where moveCursor :: Direction -> GameState ()
          moveCursor dir = do
              tl <- use targetLoc
              let tl' = tl <+> offsetDir dir
                  inGame = inBounds tl'
              when inGame $ targetLoc .= tl'

inventoryScreen :: Char -> (Item -> Bool) -> (Item -> GameState()) -> GameState ()
inventoryScreen key filt action = do
    updated .= False
    mi <- selectedItem key filt
    when (isJust mi) $ do
        let i = fromJust mi
        updated .= True
        dropScreen
        action i
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
    let vi = filter (\i -> i^.itemGlyph == '*') inv
    when (null vi) $ lose "You attempt to escape without the idol. The cave entrance collapses on your face."
    unless (null vi) win

climb :: Climb -> GameState ()
climb dir = do
    p <- use player
    let loc@(_,_,depth) = p^.location
    tile <- tileAt' loc
    when (tile == stairsDown && dir == Down) $
        move p $ offsetClimb dir
    when (tile == stairsUp && dir == Up) $ do
        when (depth == 0)
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
