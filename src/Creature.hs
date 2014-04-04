module Creature
    ( creatureTick
    , move
    , playerPickup
    , playerDropItem
    , creatureAttack
    , creatureDefense
    , playerEquip
    , eat
    , quaff
    , levelUpStrings
    , levelUpActions
    , playerThrowAttack
    , playerRangedAttack
    , gainHealth
    , loseHealth
    , action
    , healthEffect
    , poisonEffect
    , warriorEffect
    , regenEffect
    , cast
    ) where

import Prelude as P hiding (floor)

import Control.Lens
import Control.Monad (when, unless, forM_)
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


creatureTick :: Creature -> GameState ()
creatureTick c = do -- creatures can be destroyed (and removed from map), so verify they are still "alive"
    cs <- use creatures
    when ((c^.c_id) `M.member` cs) $ do
        let c' = cs M.! (c^.c_id)
        effectTick c' >>= manaTick >>= creatureTick' (c^.c_kind)

manaTick :: Creature -> GameState Creature
manaTick c = do
    when ((c^.maxMana) > 0) $ updateCreatureS c $ do
        cd <- manaRegenCooldown <+= 1
        tpr <- use tickPerManaRegen
        when (cd >= tpr) $ do
            manaRegenCooldown .= 0
            m <- mana <+= 1
            mm <- use maxMana
            when (m > mm) $ mana .= mm
    use $ creatures .to (M.! (c^.c_id))

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
    pl <- use $ player.location
    playerVisible <- canSee' pl z
    if playerVisible then hunt z else wander z
    autoLevelUp z

creatureTick' Goblin g = do
    pl <- use $ player.location
    playerVisible <- canSee' pl g
    canPickup <- creatureCanPickup g
    let canEquip = not (P.null (g^.inventory))
    when canEquip $ equip (head $ g^.inventory) g
    if playerVisible then hunt g
    else if canPickup then
        pickup g
    else wander g
    autoLevelUp g

effectTick :: Creature -> GameState Creature
effectTick c = do
    let es = c^.effects .to M.elems
    forM_ es $ \e -> do
        if effectDone e then do
            updateCreatureS c $ effects %= M.delete (e^.effectId)
            c' <- getC c
            e^.endEffect $ c'
        else updateCreatureS c $ effects %= M.adjust (effectTime +~ 1) (e^.effectId)
        c' <- getC c
        unless (effectDone e) $ e^.updateEffect $ c'
    getC c
    where getC c = use $ creatures .to (M.! (c^.c_id))

creatureCanPickup :: Creature -> GameState Bool
creatureCanPickup c = do
    let loc = c^.location
    mi <- use $ items .to (M.lookup loc)
    return $ case mi of
                  Nothing -> False
                  Just i -> isValuable i

    where isValuable i = i^.i_attackPower > 0 || i^.i_defensePower > 0

pickup :: Creature -> GameState ()
pickup c = do
    pl <- use $ player.location
    let loc = c^.location
        fullInv = inventoryFull c
    itemThere <- isItem loc
    when (not fullInv && itemThere) $ do
        item <- itemAt loc
        removeItemFromWorld loc
        updateCreatureS c $ inventory %= (item:)
        act <- action c "pickup"
        notify pl $ act ++ " a " ++ (item^.itemName)

hunt :: Creature -> GameState ()
hunt c = do
    (px,py,pz) <- use $ player.location
    let (x,y,_) = c^.location
        ln = line (x,y) (px, py)
        (x', y') = ln !! 1
    moveAbs c (x',y',pz)

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
            creatures %= insert (child^.c_id) child


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

levelUpHealth = do
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
                levelUpHealth
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
commonAttack creature other maxAttack =
    when (maxAttack > 0) $ do
        attackValue <- randomR (1, maxAttack)
        let other' = (hp -~ attackValue) other
        attackStr <- action creature "attack"
        targetStr <- target other
        notify (creature^.location) $ attackStr ++ " " ++ targetStr ++ " for " ++ show attackValue ++ " damage."
        let dieAction = do
                die other' creature
                let xpGain = xpOf other' - 2 * (creature^.level)
                when (xpGain > 0) $ modifyXP xpGain creature
        if other'^.hp < 1 then dieAction
                          else updateCreature other'

playerThrowAttack :: Item -> Creature -> GameState ()
playerThrowAttack item other = do
    p <- use player
    let maxAttack = div (p^.attack_power) 2 + item^.i_throwAttackPower - creatureDefense other
    other' <- case item^.quaffEffect of
                   Nothing -> return other
                   Just e -> addEffect e other
    commonAttack p other' maxAttack

playerRangedAttack :: Creature -> GameState ()
playerRangedAttack other = do
    p <- use player
    let Just w = p^.weapon
        maxAttack = div (p^.attack_power) 2 + w^.i_rangedAttackPower - creatureDefense other
    commonAttack p other maxAttack

attack :: Creature -> Creature -> GameState()
attack creature other = do
    let maxAttack = creatureAttack  creature - creatureDefense other
    commonAttack creature other maxAttack

die :: Creature -> Creature -> GameState ()
die c other = do
    isPlayer <- use $ player .to (c==)
    if isPlayer then lose $ "You were killed by a " ++ other^.name
    else do
        dropCorpse c
        creatureDie c

creatureDie :: Creature -> GameState ()
creatureDie c = do
    creatures %= M.delete (c^.c_id)
    let msg name = "The " ++ name ++ " dies."
    notify (c^.location) $ msg (c^.name)

foodValue :: Creature -> Int
foodValue c =
    let afv = (c^.level) * 100 + (c^.attack_power) * 10 + (c^.defense) * 10
        fv = if (c^.c_kind) == Zombie then -afv else afv
        in fv

dropCorpse :: Creature -> GameState ()
dropCorpse c = if hasItem c then dropItem c else dropCorpse' c

    where hasItem c = not $ P.null $ c^.inventory

          dropItem c = do
              let item = c^.inventory .to head
              creatureDropItem item c

          dropCorpse' c = do
            r <- randomR (1, 100)
            when (r > 50) $ do
                id <- nextInt
                let fv = foodValue c
                    corpse = Item { _itemName = (c^.name) ++ " corpse"
                                  , _itemGlyph = 'c'
                                  , _itemStyle = c^.c_style
                                  , _itemId = id
                                  , _itemLocation = c^.location
                                  , _i_attackPower = 0
                                  , _i_defensePower = 0
                                  , _i_foodValue = fv
                                  , _i_throwAttackPower = 0
                                  , _i_rangedAttackPower = 0
                                  , _quaffEffect = Nothing
                                  , _itemSpells = []
                                  }
                items %= insert (corpse^.itemLocation) corpse

updateCreature :: Creature -> GameState ()
updateCreature c = creature c .= c

updateCreatureS :: Creature -> CreatureState a -> GameState ()
updateCreatureS c cs = creature c %= execState cs

move :: Creature -> Coord -> GameState ()
move creature offset = do
    let origin = creature^.location
        move origin = origin <+> offset
        loc = move origin
    moveAbs creature loc

moveAbs :: Creature -> Coord -> GameState ()
moveAbs creature loc = when (inBounds loc) $ do
    mc <- creatureAt loc
    canMove <- canMove loc
    p <- use player
    case mc of
         Just other -> attack creature other
         Nothing | creature == p  -> if canMove then updateLoc loc else dig loc
                 | canMove -> creatures %= adjust (location .~ loc) (creature^.c_id)
                 | otherwise ->  return ()

    where updateLoc loc = do
              player.location .= loc
              targetLoc .= loc

dig :: Coord -> GameState ()
dig loc = do
    world %= (//[(reverseCoord loc, floor)])
    minusFood 10
    notify loc "You dig."

minusFood :: Int -> GameState ()
minusFood val = do
    f <- player.food <-= val
    when (f < 1) $ lose "You starved to death."

canMove :: Coord -> GameState Bool
canMove loc = do
    tile <- tileAt' loc
    return $ tile^.kind `elem` [ Floor, StairsUp, StairsDown ]

inventoryFull :: Creature -> Bool
inventoryFull c = length (c^.inventory) == c^.maxInv

playerPickup :: GameState ()
playerPickup = do
    c <- use player
    pickup c

creatureDropItem :: Item -> Creature -> GameState ()
creatureDropItem item c = do
    let loc = c^.location
    updateCreatureS c $ do
        inventory %= L.delete item
        creatureUnequip item
    act <- action c "drop"
    notify loc $ act ++ " the " ++ (item^.itemName)
    items %= insert loc (item {_itemLocation = loc})

creatureUnequip :: Item -> CreatureState ()
creatureUnequip i = do
    mweap <- use weapon
    marm <- use armor
    let isWeap = mweap == Just i
        isArm = marm == Just i
    when isWeap $ weapon .= Nothing
    when isArm $ armor .= Nothing


playerDropItem :: Item -> GameState ()
playerDropItem item = do
    loc <- use $ player.location
    player.inventory %= L.delete item
    notify loc $ "You drop the " ++ (item^.itemName)
    items %= insert loc (item {_itemLocation = loc})
    unequip item

playerEquip :: Item -> GameState ()
playerEquip i = do
    p <- use player
    equip i p

equip :: Item -> Creature -> GameState ()
equip i c = do
    let loc = c^.location
    act <- action c "equip"
    notify loc $ act ++ " the " ++ i^.itemName
    updateCreatureS c $ equip' i

    where equip' i = do
              let ap = i^.i_attackPower
                  dp = i^.i_defensePower
              if ap >= dp
                 then weapon .= Just i
                 else armor .= Just i

unequip :: Item -> GameState ()
unequip i = do
    mweap <- use $ player.weapon
    marm <- use $ player.armor
    let isWeap = mweap == Just i
        isArm = marm == Just i
    when isWeap $ player.weapon .= Nothing
    when isArm $ player.armor .= Nothing

eat :: Item -> GameState ()
eat i = do
    max <- use $ player.maxFood
    loc <- use $ player.location
    notify loc $ "You eat the " ++ i^.itemName
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

gainHealth :: Int -> Creature -> GameState ()
gainHealth val c =
    updateCreatureS c $ do
        hp' <- hp <+= val
        max <- use maxHp
        when (hp' > max) $ hp .= max

loseHealth :: Int -> Creature -> GameState ()
loseHealth val c = do
    let hp' = c^.hp - val
    updateCreatureS c $ hp .= hp'
    when (hp' <= 0) $ do
        isPlayer <- use $ player .to (c==)
        if isPlayer then lose "You were killed by unknown forces"
                    else creatureDie c

addEffect :: Effect -> Creature -> GameState Creature
addEffect e c = do
    e^.startEffect $ c
    return $ if effectDone e
                then c
                else (effects %~ insert (e^.effectId) e) c

quaff :: Item -> GameState ()
quaff i = do
    let Just e = i^.quaffEffect
    p <- use player
    player.inventory %= L.delete i
    applyEffect e p

applyEffect :: Effect -> Creature -> GameState ()
applyEffect e c = do
    e^.startEffect $ c
    unless (effectDone e) $ updateCreatureS c $
        effects %= insert (e^.effectId) e

defaultEffect = Effect
    { _startEffect = const $ return ()
    , _endEffect = const $ return ()
    , _updateEffect = const $ return ()
    , _effectDuration = 0
    , _effectTime = 0
    , _effectId = 0
    }

creatureNotify :: String -> Creature -> GameState ()
creatureNotify str c = do
    acs <- action c str
    notify (c^.location) acs

warriorEffect = defaultEffect
    { _startEffect = \c -> creatureNotify "feel stronger" c >> updateCreatureS c increasePower
    , _endEffect = \c -> creatureNotify "feel weaker" c >> updateCreatureS c decreasePower
    , _effectDuration = 20
    }

    where increasePower = do
              attack_power += 5
              defense += 5
          decreasePower = do
              attack_power -= 5
              defense -= 5


poisonEffect = defaultEffect
    { _updateEffect = loseHealth 1
    , _startEffect = creatureNotify "feel sick"
    , _endEffect = creatureNotify "feel better"
    , _effectDuration = 5
    }

healthEffect val = defaultEffect
    { _startEffect = gainHealth val
    }

regenEffect val = defaultEffect
    { _updateEffect = gainHealth val
    , _startEffect = creatureNotify "start regenerating"
    , _endEffect = creatureNotify "stop regenerating"
    , _effectDuration = 20
    }

-- TODO -- effect id should be generated at cast time (each spell cast has a different effect)
-- theoretically, the same spell effect could be applied multiple times
cast :: Spell -> GameState ()
cast spell = do
    let e = spell^.spellEffect
    m <- use $ player.mana
    when (m >= spell^.manaCost) $ do
        tl <- use targetLoc
        player.mana -= spell^.manaCost
        mc <- creatureAt tl
        case mc of
             Nothing -> notify tl $ spell^.spellName ++ " fizzles."
             Just c -> do
                 notify tl $ "casting spell " ++ spell^.spellName ++ " at " ++ c^.name
                 e^.startEffect $ c
                 when (effectDone e) $ do
                     use (creature c) >>= e^.endEffect
                 creature c %= execState (castOnCreature e)
    where castOnCreature e =
              unless (effectDone e) $
                 effects %= insert (e^.effectId) e
