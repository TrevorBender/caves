module Creature where

import Prelude hiding (floor)

import Control.Lens
import Control.Monad (when)
import Control.Monad.State.Strict (execState, get, runState)
import Data.Map.Strict as M (fromList, union, insert, delete)

import Game
import Random
import Generation (createFungus)
import World (isFloor, isCreature)

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
            when (isFloor && not isCreature) $ do
                fungus <- createFungus depth
                let fungus' = (location .~ loc') fungus
                creatures %= (insert (fungus^.c_id) fungus')

creatureTick' _ _ = return ()

attack :: Creature -> Creature -> GameState()
attack creature other = do
    let ap = creature^.attack_power
        other' = (hp -~ ap) other
    if other'^.hp < 1 then die other'
                      else updateCreature other'

die :: Creature -> GameState ()
die c = do
    game <- get
    let cs = delete (c^.c_id) (game^.creatures)
    creatures .= cs

updateCreature :: Creature -> GameState ()
updateCreature c = do
    game <- get
    let cs = insert (c^.c_id) c (game^.creatures)
    creatures .= cs

