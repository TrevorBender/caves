module Creature
    ( creatureTick
    , move
    , playerPickup
    , playerDropItem
    , creatureAttack
    , creatureDefense
    , equip
    , createPlayer
    , createFungus
    , createBat
    ) where

import Prelude hiding (floor)

import Control.Lens
import Control.Monad (when, unless)
import Control.Monad.State.Strict (execState, get, runState)
import Data.Map.Strict as M
import Data.Maybe (isJust, fromJust)
import Data.Array as A

import Game
import Random
import World ( isFloor, isCreature
             , isItem, seeThrough
             , creatureAt, floor
             , tileAt, itemAt
             , removeItemFromWorld
             , findEmptyLocation
             )
import Line (line)

emptyInventory = []

creatureDefaults :: Creature
creatureDefaults = Creature { _location = (0,0,0)
                            , _c_kind = Player
                            , _c_glyph = 'X'
                            , _c_style = DefaultStyle
                            , _c_id = -1
                            , _name = "<fixme: default>"
                            , _attack_power = 0
                            , _defense = 1
                            , _hp = 1
                            , _maxHp = 1
                            , _visionRadius = 20
                            , _inventory = emptyInventory
                            , _maxInv = 0
                            , _weapon = Nothing
                            , _armor = Nothing
                            }

createPlayer :: Creature
createPlayer = creatureDefaults
    { _c_kind = Player
    , _c_glyph = '@'
    , _c_style = PlayerStyle
    , _c_id = 0
    , _name = "<fixme: you>"
    , _attack_power = 10
    , _defense = 1
    , _hp = 40
    , _maxHp = 40
    , _maxInv = 20
    }

creature :: Creature -> Int -> GameState Creature
creature constructor depth = do
    loc <- findEmptyLocation depth
    thisId <- nextInt
    return $ constructor { _location = loc , _c_id = thisId }

createFungus :: Int -> GameState Creature
createFungus = creature creatureDefaults
    { _c_kind = Fungus
    , _c_glyph = 'f'
    , _c_style = FungusStyle
    , _name = "lichen"
    , _defense = 1
    , _hp = 1
    , _maxHp = 1
    }

createBat :: Int -> GameState Creature
createBat = creature creatureDefaults
        { _c_kind = Bat
        , _c_glyph = 'b'
        , _c_style = BatStyle
        , _name = "bat"
        , _attack_power = 4
        , _defense = 4
        , _hp = 5
        , _maxHp = 5
        }

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

itemDefensePower :: Creature -> Int
itemDefensePower c = weapDef + armDef
    where weapDef = maybe 0 _i_defensePower $ c^.weapon
          armDef  = maybe 0 _i_defensePower $ c^.armor

itemAttackPower :: Creature -> Int
itemAttackPower c = weapVal + armVal
    where weapVal = maybe 0 _i_attackPower (c^.weapon)
          armVal  = maybe 0 _i_attackPower (c^.armor)

creatureAttack :: Creature -> Int
creatureAttack c = c^.attack_power + itemAttackPower c

creatureDefense :: Creature -> Int
creatureDefense c = c^.defense + itemDefensePower c

attack :: Creature -> Creature -> GameState()
attack creature other = get >>= \game -> do
    let maxAttack = creatureAttack  creature - creatureDefense other
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
    if isPlayer then lose
    else do
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

playerPickup :: GameState ()
playerPickup = do
    game <- get
    let c = game^.player
    let loc = c^.location
        fullInv = inventoryFull c
    itemThere <- isItem loc
    when (not fullInv && itemThere) $ do
        item <- itemAt loc
        removeItemFromWorld loc
        player.inventory %= (item:)
        notify (game^.player.location) $ "You pickup a " ++ (item^.i_name)

playerDropItem :: Int -> GameState ()
playerDropItem ix = do
    game <- get
    let item = (game^.player.inventory) !! ix
    player.inventory %= (remove ix)
    notify (game^.player.location) $ "You drop the " ++ (item^.i_name)
    let loc = game^.player.location
    items %= insert loc (item {_i_location = loc})
    unequip item

    where remove ix xs = let (bs, cs) = splitAt ix xs
                         in bs ++ tail cs

equip :: Item -> GameState ()
equip i = do
    let ap = i^.i_attackPower
        dp = i^.i_defensePower
    game <- get
    notify (game^.player.location) $ "You equip the " ++ i^.i_name
    if ap >= dp
       then player.weapon .= Just i
       else player.armor .= Just i

unequip :: Item -> GameState ()
unequip i = do
    game <- get
    let mweap = game^.player.weapon
        marm = game^.player.armor
        isWeap = isJust mweap && fromJust mweap == i
        isArm = isJust marm && fromJust marm == i
    when isWeap $ player.weapon .= Nothing
    when isArm $ player.armor .= Nothing
