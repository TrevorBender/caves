module Creature where

import Prelude hiding (floor)

import Control.Lens
import Control.Monad (when, unless)
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

action :: Creature -> String -> GameState String
action c action = get >>= \game -> return $ if c == game^.player then "You " ++ action else c^.name ++ action ++ "s"

target :: Creature -> GameState String
target c = get >>= \game -> return $ if c == game^.player then "you" else "the " ++ c^.name

attack :: Creature -> Creature -> GameState()
attack creature other = get >>= \game -> do
    let maxAttack = creature^.attack_power - other^.defense
    attackValue <- randomR (1, maxAttack)
    let other' = (hp -~ attackValue) other
    attackStr <- action creature "attack"
    targetStr <- target other
    notify $ attackStr ++ " " ++ targetStr ++ " for " ++ show attackValue ++ " damage."
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
        notify $ msg (c^.name)

updateCreature :: Creature -> GameState ()
updateCreature c = do
    game <- get
    let cs = insert (c^.c_id) c (game^.creatures)
    creatures .= cs

