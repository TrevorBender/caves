module Creature
    ( creatureTick
    , move
    , playerPickup
    ) where

import Prelude hiding (floor)

import Control.Lens
import Control.Monad (when, unless)
import Control.Monad.State.Strict (execState, get, runState)
import Data.Map.Strict as M
import Data.Array as A

import Game
import Random
import Generation (createFungus)
import World ( isFloor, isCreature
             , isItem, seeThrough
             , creatureAt, floor
             , tileAt, itemAt
             , removeItemFromWorld
             )
import Line (line)

creatureTick :: Creature -> GameState ()
creatureTick c = creatureTick' (c^.c_kind) c

creatureTick' :: CreatureKind -> Creature -> GameState ()

creatureTick' Fungus c = do
    game <- get
    let loc@(_,_,depth) = c^.location
    r <- randomR (0,99)
    when (r == 99) $ do
        dir <- randomL [N,E,S,W,NE,SE,SW,NW]
        let offset = offsetDir dir
            loc' = offset <+> loc
        when (inBounds loc') $ do
            isFloor <- isFloor loc'
            isCreature <- isCreature loc'
            isItem <- isItem loc'
            when (isFloor && not isCreature && not isItem) $ do
                fungus <- createFungus depth
                let fungus' = (location .~ loc') fungus
                creatures %= (insert (fungus^.c_id) fungus')

creatureTick' Bat bat = do
    game <- get
    dir <- randomL [N,E,S,W,NE,SE,SW,NW]
    let offset = offsetDir dir
    move bat offset

creatureTick' _ _ = return ()

action :: Creature -> String -> GameState String
action c action = get >>= \game -> return $ if c == game^.player
                                               then "You " ++ action
                                               else c^.name ++ " " ++  action ++ "s"

target :: Creature -> GameState String
target c = get >>= \game -> return $ if c == game^.player then "you" else "the " ++ c^.name

attack :: Creature -> Creature -> GameState()
attack creature other = get >>= \game -> do
    let maxAttack = creature^.attack_power - other^.defense
    attackValue <- randomR (1, maxAttack)
    let other' = (hp -~ attackValue) other
    attackStr <- action creature "attack"
    targetStr <- target other
    notify (creature^.location) $ attackStr ++ " " ++ targetStr ++ " for " ++ show attackValue ++ " damage."
    if other'^.hp < 1 then die other'
                      else updateCreature other'

die :: Creature -> GameState ()
die c = do
    game <- get
    let isPlayer = c == game^.player
    when isPlayer $ lose
    unless isPlayer $ do
        let cs = delete (c^.c_id) (game^.creatures)
        creatures .= cs
        let msg name = "The " ++ name ++ " dies."
        notify (c^.location) $ msg (c^.name)

updateCreature :: Creature -> GameState ()
updateCreature c = do
    game <- get
    let cs = insert (c^.c_id) c (game^.creatures)
    creatures .= cs

move :: Creature -> Coord -> GameState ()
move creature offset = do
    game <- get
    let origin = creature^.location
        move origin = origin <+> offset
        loc = move origin
        p = game^.player
    when (inBounds loc) $ do
        mc <- creatureAt loc
        canMove <- canMove loc
        case mc of
             Just other -> attack creature other
             Nothing ->
                 if creature == p then
                    if canMove
                       then (player.location) .= loc
                       else dig loc
                    else if canMove
                       then creatures %= (adjust (location .~ loc) (creature^.c_id))
                       else return ()
    return ()

dig :: Coord -> GameState ()
dig loc = do
    game <- get
    world %= (//[(reverseCoord loc, floor)])
    notify (game^.player.location) $ "You dig."

canMove :: Coord -> GameState Bool
canMove loc = do
    game <- get
    let tile = tileAt game loc
    return $ tile^.kind `elem` [ Floor, StairsUp, StairsDown ]

inventoryFull :: Creature -> Bool
inventoryFull c = length (c^.inventory) == c^.maxInv

pickup :: Creature -> GameState ()
pickup c = do
    game <- get
    let loc = c^.location
        fullInv = inventoryFull c
    hasItem <- isItem loc
    when (not fullInv && hasItem) $ do
        item <- itemAt loc
        removeItemFromWorld loc
        player.inventory %= (item:)

playerPickup :: GameState ()
playerPickup = do
    game <- get
    pickup (game^.player)
