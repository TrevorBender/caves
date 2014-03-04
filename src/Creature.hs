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
    , createZombie
    , eat
    , levelUpStrings
    , levelUpActions
    , playerThrowAttack
    ) where

import Prelude as P hiding (floor)

import Control.Lens
import Control.Monad (when, unless)
import Control.Monad.State.Strict (execState, get, runState)
import Data.List as L (delete)
import Data.Map.Strict as M
import Data.Maybe (isJust, fromJust)
import Data.Array as A

import Game
import Random
import World ( isFloor, isCreature
             , isItem, seeThrough
             , creatureAt, floor
             , tileAt', itemAt
             , removeItemFromWorld
             , findEmptyLocation
             , canSee'
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
                            , _food = 0
                            , _maxFood = 0
                            , _xp = 0
                            , _level = 1
                            , _levelUpgrades = 0
                            }

createPlayer :: Creature
createPlayer = creatureDefaults
    { _c_kind = Player
    , _c_glyph = '@'
    , _c_style = PlayerStyle
    , _c_id = 0
    , _name = "You"
    , _attack_power = 10
    , _defense = 1
    , _hp = 40
    , _maxHp = 40
    , _maxInv = 20
    , _maxFood = 1000
    , _food = div 1000 3 * 2
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

createZombie :: Int -> GameState Creature
createZombie = creature creatureDefaults
    { _c_kind = Zombie
    , _c_glyph = 'z'
    , _c_style = ZombieStyle
    , _name = "zombie"
    , _attack_power = 15
    , _defense = 5
    , _hp = 15
    , _maxHp = 15
    , _visionRadius = 15
    }

creatureTick :: Creature -> GameState ()
creatureTick c = creatureTick' (c^.c_kind) c

creatureTick' :: CreatureKind -> Creature -> GameState ()

creatureTick' Fungus c = do
    r <- randomR (0,99)
    when (r == 99) $ duplicate c

creatureTick' Bat bat = do
    wander bat
    autoLevelUp bat

creatureTick' Player p = do
    minusFood 1
    let ups = p^.levelUpgrades
    when (ups > 0) $ pushScreen ChooseLevelUp

creatureTick' Zombie z = do
    pl@(px,py,pz) <- use $ player.location
    playerVisible <- canSee' pl z
    let hunt = do
            let (x,y,_) = z^.location
                ln = line (x,y) (px, py)
                (x', y') = ln !! 1
            moveAbs z (x',y',pz)
    if playerVisible then hunt else wander z
    autoLevelUp z


duplicate :: Creature -> GameState ()
duplicate c = do
    let loc@(_,_,depth) = c^.location
    dir <- randomL [N,E,S,W,NE,SE,SW,NW]
    let offset = offsetDir dir
        loc' = offset <+> loc
    when (inBounds loc') $ do
        isFloor <- isFloor loc'
        isCreature <- isCreature loc'
        isItem <- isItem loc'
        when (isFloor && not isCreature && not isItem) $ do
            id <- nextInt
            let child = c { _c_id = id , _location = loc' }
            creatures %= (insert (child^.c_id) child)


upgradeMaxHp = do
    hp += 10
    maxHp += 10
    levelUpgrades -= 1
upgradeAttack = do
    attack_power += 2
    levelUpgrades -= 1
upgradeDefense = do
    defense += 2
    levelUpgrades -= 1
upgradeVision = do
    visionRadius += 2
    levelUpgrades -= 1

upgrades = [ (upgradeMaxHp, "look healthier")
           , (upgradeAttack, "look stronger")
           , (upgradeDefense, "look tougher")
           , (upgradeVision, "look more aware")
           ]

gainHealth = do
    mh <- use maxHp
    let mhf = fromIntegral mh :: Float
        hu = mhf * 0.1
    h <- hp <+= P.round hu
    when (h > mh) $ hp .= mh

data LevelUpOption = LevelUpOption
    { levelUpDescription :: String
    , levelUpAction :: CreatureState ()
    }

increaseHitPoints = LevelUpOption "Increased hit points" upgradeMaxHp
increaseAttackPower = LevelUpOption "Increased attack power" upgradeAttack
increaseToughness = LevelUpOption "Increase toughness" upgradeDefense
increaseVision = LevelUpOption "Increase vision" upgradeVision
levelUpOptions = [ increaseHitPoints
                 , increaseAttackPower
                 , increaseToughness
                 , increaseVision
                 ]

levelUpStrings :: [String]
levelUpStrings = P.map (\(c, s) -> c:' ':s) . zip ['a'..] . P.map levelUpDescription $ levelUpOptions

levelUpActions :: [(Char, CreatureState ())]
levelUpActions = zip ['a'..] . P.map levelUpAction $ levelUpOptions

autoLevelUp :: Creature -> GameState ()
autoLevelUp c = do
    let lu = c^.levelUpgrades
    when (lu > 0) $ do
        (upgrade, str) <- randomL upgrades
        let autoUp = do
                gainHealth
                upgrade
        action c str >>= notify (c^.location)
        updateCreature $ execState autoUp c

wander :: Creature -> GameState ()
wander c = do
    dir <- randomL [N,E,S,W,NE,SE,SW,NW]
    let offset = offsetDir dir
    move c offset

action :: Creature -> String -> GameState String
action c action = do
    p <- use player
    return $ if c == p
                then "You " ++ action
                else c^.name ++ " " ++  action ++ "s"

target :: Creature -> GameState String
target c = do
    p <- use player
    return $ if c == p then "you" else "the " ++ c^.name

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

commonAttack :: Creature -> Creature -> Int -> GameState ()
commonAttack creature other maxAttack = do
    when (maxAttack > 0) $ do
        attackValue <- randomR (1, maxAttack)
        let other' = (hp -~ attackValue) other
        attackStr <- action creature "attack"
        targetStr <- target other
        notify (creature^.location) $ attackStr ++ " " ++ targetStr ++ " for " ++ show attackValue ++ " damage."
        let dieAction = do
                die other'
                let xpGain = xpOf other' - 2 * (creature^.level)
                when (xpGain > 0) $ modifyXP xpGain creature
        if other'^.hp < 1 then dieAction
                          else updateCreature other'

playerThrowAttack :: Item -> Creature -> GameState ()
playerThrowAttack item other = do
    p <- use player
    let maxAttack = div (p^.attack_power) 2 + item^.i_throwAttackPower - creatureDefense other
    commonAttack p other maxAttack

attack :: Creature -> Creature -> GameState()
attack creature other = do
    let maxAttack = creatureAttack  creature - creatureDefense other
    commonAttack creature other maxAttack

die :: Creature -> GameState ()
die c = do
    isPlayer <- use $ player .to (c==)
    if isPlayer then lose
    else do
        r <- randomR (1, 100)
        when (r > 50) $ dropCorpse c
        creatures %= M.delete (c^.c_id)
        let msg name = "The " ++ name ++ " dies."
        notify (c^.location) $ msg (c^.name)

foodValue :: Creature -> Int
foodValue c =
    let afv = (c^.level) * 100 + (c^.attack_power) * 10 + (c^.defense) * 10
        fv = if (c^.c_kind) == Zombie then -afv else afv
        in fv

dropCorpse :: Creature -> GameState ()
dropCorpse c = do
    id <- nextInt
    let fv = foodValue c
        corpse = Item { _i_name = (c^.name) ++ " corpse"
                      , _i_glyph = 'c'
                      , _i_style = c^.c_style
                      , _i_id = id
                      , _i_location = c^.location
                      , _i_attackPower = 0
                      , _i_defensePower = 0
                      , _i_foodValue = fv
                      , _i_throwAttackPower = 0
                      }
    items %= insert (corpse^.i_location) corpse

updateCreature :: Creature -> GameState ()
updateCreature c = creatures %= insert (c^.c_id) c

move :: Creature -> Coord -> GameState ()
move creature offset = do
    let origin = creature^.location
        move origin = origin <+> offset
        loc = move origin
    moveAbs creature loc

-- TODO refactor deep nesting
moveAbs :: Creature -> Coord -> GameState ()
moveAbs creature loc = do
    when (inBounds loc) $ do
        mc <- creatureAt loc
        canMove <- canMove loc
        p <- use player
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
    world %= (//[(reverseCoord loc, floor)])
    minusFood 10
    notify loc $ "You dig."

minusFood :: Int -> GameState ()
minusFood val = do
    f <- player.food <-= val
    if f < 1 then lose else return ()

canMove :: Coord -> GameState Bool
canMove loc = do
    tile <- tileAt' loc
    return $ tile^.kind `elem` [ Floor, StairsUp, StairsDown ]

inventoryFull :: Creature -> Bool
inventoryFull c = length (c^.inventory) == c^.maxInv

playerPickup :: GameState ()
playerPickup = do
    c <- use player
    let loc = c^.location
        fullInv = inventoryFull c
    itemThere <- isItem loc
    when (not fullInv && itemThere) $ do
        item <- itemAt loc
        removeItemFromWorld loc
        player.inventory %= (item:)
        notify loc $ "You pickup a " ++ (item^.i_name)

playerDropItem :: Item -> GameState ()
playerDropItem item = do
    loc <- use $ player.location
    player.inventory %= (remove item)
    notify loc $ "You drop the " ++ (item^.i_name)
    items %= insert loc (item {_i_location = loc})
    unequip item

    where remove item = P.filter (\i -> (i^.i_id) /= (item^.i_id))

equip :: Item -> GameState ()
equip i = do
    let ap = i^.i_attackPower
        dp = i^.i_defensePower
    loc <- use $ player.location
    notify loc $ "You equip the " ++ i^.i_name
    if ap >= dp
       then player.weapon .= Just i
       else player.armor .= Just i

unequip :: Item -> GameState ()
unequip i = do
    mweap <- use $ player.weapon
    marm <- use $ player.armor
    let isWeap = isJust mweap && fromJust mweap == i
        isArm = isJust marm && fromJust marm == i
    when isWeap $ player.weapon .= Nothing
    when isArm $ player.armor .= Nothing

eat :: Item -> GameState ()
eat i = do
    max <- use $ player.maxFood
    player.food += (i^.i_foodValue)
    player.food %= \f -> if f > max then max else f
    player.inventory %= L.delete i

modifyXP :: Int -> Creature -> GameState ()
modifyXP val c = updateCreature $ execState (modifyXp' val) c

    where modifyXp' val = do
              x <- xp <+= val
              l <- use $ level .to fromIntegral
              when (fromIntegral x > l ** 1.5 * 20.0) levelUp

          levelUp = do
              level += 1
              levelUpgrades += 1

isPlayer :: Creature -> Bool
isPlayer c = (c^.c_kind) == Player

xpOf :: Creature -> Int
xpOf c =
    let mhp = c^.maxHp
        av  = creatureAttack c
        dv  = creatureDefense c
        in mhp + av + dv

