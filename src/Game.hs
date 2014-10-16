{-# LANGUAGE TemplateHaskell #-}

module Game where

import Control.Lens
import Control.Monad (when)
import Control.Monad.State.Strict (State, get, MonadState)
import Data.Array as A
import Data.Map.Strict as M (Map, (!), insert, fromAscList, lookup)
import Data.Maybe (isJust)
import System.Random (StdGen)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper

gameWidth, gameHeight, gameDepth :: Int
gameWidth = 90
gameHeight = 30
gameDepth = 5

debugOn = False

data Screen = Start | Win | Lose | Play | DropItem | EquipItem | EatItem | ChooseLevelUp
            | Help | ExamineItem | Look | Throw | ThrowItem | FireWeapon | QuaffItem
            | CastSpell     -- pick target for spell
            | ReadItem      -- pick spell book
            | ReadSpellBook -- pick a spell

type Coord = (Int, Int, Int)

data StyleType = DefaultStyle
               | PlayerStyle
               | FungusStyle
               | OutOfSiteStyle
               | BatStyle
               | VictoryItemStyle
               | SwordStyle
               | StaffStyle
               | ZombieStyle
               deriving (Eq, Ord)

styleMap :: Map StyleType Style
styleMap = M.fromAscList
         [ (DefaultStyle, defaultStyle)
         , (PlayerStyle, AttributeStyle [Bold] DefaultF DarkBlueB)
         , (FungusStyle, AttributeStyle [Bold] GreenF DefaultB)
         , (OutOfSiteStyle, AttributeStyle [Bold] WhiteF BlackB)
         , (BatStyle, AttributeStyle [Bold] BrownF DefaultB)
         , (VictoryItemStyle, AttributeStyle [Bold] YellowF DefaultB)
         , (SwordStyle, AttributeStyle [] CyanF DefaultB)
         , (StaffStyle, AttributeStyle [] BrownF DefaultB)
         , (ZombieStyle, AttributeStyle [Bold] RedF DefaultB)
         ]

data Effect_ c g = Effect
    { _startEffect :: c -> g
    , _updateEffect :: c -> g
    , _endEffect :: c -> g
    , _effectDuration :: Int
    , _effectTime :: Int
    , _effectId :: Int
    }
makeLenses ''Effect_

instance Eq (Effect_ c g) where
    (==) a b = a^.effectId == b^.effectId

data Spell_ c g = Spell
    { _spellName :: String
    , _manaCost :: Int
    , _spellEffect :: Effect_ c g
    }
makeLenses ''Spell_

data Item_ c g = Item
    { _itemGlyph :: Char
    , _itemStyle :: StyleType
    , _itemId :: Int
    , _itemName :: String
    , _itemLocation :: Coord
    , _iAttackPower :: Int
    , _iDefensePower :: Int
    , _iFoodValue :: Int
    , _iThrowAttackPower :: Int
    , _iRangedAttackPower :: Int
    , _quaffEffect :: Maybe (Effect_ c g)
    , _itemSpells :: [Spell_ c g]
    }
makeLenses ''Item_

instance Eq (Item_ c g) where
    (==) a b = a^.itemId == b^.itemId

data CreatureKind = Player | Fungus | Bat | Zombie | Goblin deriving (Eq)
data Creature_ g = Creature
    { _location :: Coord
    , _cGlyph :: Char
    , _cStyle :: StyleType
    , _cId :: Int
    , _cKind :: CreatureKind
    , _name :: String
    , _attackPower :: Int
    , _defense :: Int
    , _hp :: Int
    , _maxHp :: Int
    , _visionRadius :: Int
    , _inventory :: [Item_ (Creature_ g) g]
    , _maxInv :: Int
    , _weapon :: Maybe (Item_ (Creature_ g) g)
    , _armor :: Maybe (Item_ (Creature_ g) g)
    , _food :: Int
    , _maxFood :: Int
    , _xp :: Int
    , _level :: Int
    , _levelUpgrades :: Int
    , _effects :: Map Int (Effect_ (Creature_ g) g)
    , _mana :: Int
    , _maxMana :: Int
    , _tickPerManaRegen :: Int
    , _manaRegenCooldown :: Int
    , _tickPerHealthRegen :: Int
    , _healthRegenCooldown :: Int
    }
makeLenses ''Creature_

instance Eq (Creature_ g) where
    (==) a b = a^.cId == b^.cId


data TileKind = Floor | Wall | StairsUp | StairsDown | Unknown deriving (Eq, Show)

data Tile = Tile
    { _kind  :: TileKind
    , _glyph :: Char
    , _tDescription :: String
    }
makeLenses ''Tile

instance Eq Tile where
    -- (==) :: Tile -> Tile -> Bool
    (==) a b = a^.kind == b^.kind

type GameWorld = Array Coord Tile

type RegionMap = ( Int                   -- current region num
                 , Map Coord (Maybe Int) -- map coord to region num
                 , Map Int [Coord]       -- map region num to coords
                 )

data Game = Game
    { _uis   :: [Screen]
    , _world :: GameWorld
    , _visibleWorld :: GameWorld
    , _creatures :: M.Map Int (Creature_ (State Game ()))
    , _items :: M.Map Coord (Item_ (Creature_ (State Game ())) (State Game ()))
    , _messages :: [String]
    , _loseMessage :: String
    , _curId :: Int     -- for id generation
    , _stdGen :: StdGen -- for random number generation
    , _updated :: Bool
    -- region map generation
    , _regionMap :: RegionMap
    , _window :: Window
    , _styles :: Map StyleType CursesStyle
    , _targetLoc :: Coord
    , _targetItem :: Maybe (Item_ (Creature_ (State Game())) (State Game ()))
    }
makeLenses ''Game

-- identical function signature:
-- player :: Functor f => (Creature -> f Creature) -> Game -> f Game
player :: Lens' Game Creature
player = creatureWithId 0

creature :: Functor f => Creature -> (Creature -> f Creature) -> Game -> f Game
creature c =
    let id = c^.cId
    in creatureWithId id

creatureWithId :: Functor f => Int -> (Creature -> f Creature) -> Game -> f Game
creatureWithId id f game =
    let updateCreature game c' = creatures %~ M.insert id c' $ game
    in fmap (updateCreature game) (f (_creatures game M.! id))

type GameState = State Game
type GameAction = GameState ()
type Creature = Creature_ GameAction
type CreatureState = State Creature
type Item = Item_ Creature GameAction
type Effect = Effect_ Creature GameAction
type Spell = Spell_ Creature GameAction

nextInt :: GameState Int
nextInt = curId <+= 1

getStyle :: StyleType -> Game -> CursesStyle
getStyle styleType game = (game^.styles) M.! styleType

ui :: GameState Screen
ui = use $ uis.to last

splitBy :: Int -> [a] -> [[a]]
splitBy width [] = []
splitBy width xs = take width xs : splitBy width (drop width xs)

data Climb = Up | Down deriving (Eq)

data Direction = N | E | S | W
               | NE | SE | SW | NW

offsetDir :: Direction -> Coord
offsetDir N = (0, -1, 0)
offsetDir E = (1,  0, 0)
offsetDir S = (0,  1, 0)
offsetDir W = (-1, 0, 0)
offsetDir NE = offsetDir N <+> offsetDir E
offsetDir SE = offsetDir S <+> offsetDir E
offsetDir SW = offsetDir S <+> offsetDir W
offsetDir NW = offsetDir N <+> offsetDir W

offsetClimb :: Climb -> Coord
offsetClimb Up   = (0, 0, -1)
offsetClimb Down = (0, 0,  1)

(<+>) :: Coord -> Coord -> Coord
(x,y,z) <+> (x',y',z') = (x + x',y + y',z+z')

inBounds :: Coord -> Bool
inBounds (x,y,z) = x >= 0
              && y >= 0
              && z >= 0
              && x < gameWidth
              && y < gameHeight
              && z < gameDepth

reverseCoord :: Coord -> Coord
reverseCoord (x, y, z) = (z, y, x)

neighborsCoords :: Coord -> [Coord]
neighborsCoords origin = ixs'
    where dirs = [N,E,S,W,NE,SE,SW,NW]
          offsets = map offsetDir dirs
          ixs = map (origin <+>) offsets
          ixs' = filter inBounds ixs

neighbors8 :: Coord -> GameWorld -> [Tile]
neighbors8 origin world = tiles
    where ixs = neighborsCoords origin
          tileAt = (world A.!) . reverseCoord
          tiles = map tileAt ixs

-- | Distance squared
distanceSq :: Coord -> Coord -> Int
distanceSq (x,y,_) (x',y',_) = sq (x - x') + sq (y - y')
    where sq x = x * x

sameDepth :: Coord -> Coord -> Bool
sameDepth (_,_,z) (_,_,z') = z == z'

notify :: Coord -> String -> GameState ()
notify loc s = do
    p <- use player
    let ploc = p^.location
        vision = p^.visionRadius
        inRange = sameDepth loc ploc && distanceSq loc ploc <= vision * vision
    when inRange $ messages %= (s:)

pushScreen :: Screen -> GameState ()
pushScreen ui = uis %= (++ [ui])

dropScreen :: GameState ()
dropScreen = uis %= safeInit
    where safeInit [] = []
          safeInit xs = init xs

lose :: String -> GameState ()
lose msg = do
    updated .= False
    loseMessage .= msg
    messages %= (msg:)
    pushScreen Lose

quit :: GameState ()
quit = do
    uis .= []
    updated .= False

win :: GameState ()
win = pushScreen Win

debug :: String -> a -> a
debug str x = if debugOn then debug' str x else x
    where debug' str x = unsafePerformIO $ do
              hPutStrLn stderr str
              return x

gameChanged :: GameState Bool
gameChanged = use updated

effectDone :: Effect -> Bool
effectDone e = e^.effectTime >= e^.effectDuration

dropItemFilter = const True
equipItemFilter i = i^.iAttackPower > 0 || i^.iDefensePower > 0
eatItemFilter i = i^.iFoodValue /= 0
examineItemFilter = const True
throwItemFilter i = i^.iThrowAttackPower > 0 || quaffItemFilter i
quaffItemFilter i = isJust $ i^.quaffEffect
readItemFilter i = not $ null $ i^.itemSpells
