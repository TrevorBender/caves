{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens
import Control.Monad.State.Strict (State, get, MonadState)
import Data.Array as A
import Data.Map.Strict as M (Map, (!), insert, fromAscList, lookup)
import System.Random (StdGen)
import UI.HSCurses.Curses (Window)
import UI.HSCurses.CursesHelper as CH (CursesStyle(..))

-- | Each screen is meant to show a different state
data Screen = Start | Win | Lose | Play | DropItem | EquipItem | EatItem | ChooseLevelUp
            | Help | ExamineItem | Look | Throw | ThrowItem | FireWeapon | QuaffItem
            | CastSpell     -- | pick target for spell
            | ReadItem      -- | pick spell book
            | ReadSpellBook -- | pick a spell

type Coord = (Int, Int, Int)

-- | A generic style for text on the screen, independent of implementation
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

-- | An effect
-- | c = a Creature (does this have to be a Creature?)
-- | g = the new game state
data Effect_ c g = Effect
    { -- | triggered when the effect is started
      _startEffect :: c -> g
      -- | triggered every game tick while the effect is active
    , _updateEffect :: c -> g
      -- | triggered at the end of the effect
    , _endEffect :: c -> g
    , _effectDuration :: Int
    , _effectTime :: Int
    , _effectId :: Int
    }
makeLenses ''Effect_

-- | Need a separate way of determining if an effect is equal.  For example, there can be multiple poison effects
-- | at the same time
instance Eq (Effect_ c g) where
    (==) a b = a^.effectId == b^.effectId

-- | A spell has an effect
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

type GameState = State Game
type GameAction = GameState ()
type Creature = Creature_ GameAction
type CreatureState = State Creature
type Item = Item_ Creature GameAction
type Effect = Effect_ Creature GameAction
type Spell = Spell_ Creature GameAction

data Climb = Up | Down deriving (Eq)

data Direction = N | E | S | W
               | NE | SE | SW | NW
