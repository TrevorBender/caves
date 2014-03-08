{-# LANGUAGE TemplateHaskell #-}

module Game where

import Control.Lens
import Control.Monad (when)
import Control.Monad.State.Strict (State, get, MonadState)
import Data.Array as A
import Data.Map.Strict as M (Map, (!), insert, fromAscList)
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
            | Help | ExamineItem | Look | Throw | ThrowItem | FireWeapon

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

data Item = Item
    { _i_glyph :: Char
    , _i_style :: StyleType
    , _i_id :: Int
    , _i_name :: String
    , _i_location :: Coord
    , _i_attackPower :: Int
    , _i_defensePower :: Int
    , _i_foodValue :: Int
    , _i_throwAttackPower :: Int
    , _i_rangedAttackPower :: Int
    }
makeLenses ''Item

instance Eq Item where
    (==) a b = a^.i_id == b^.i_id

data CreatureKind = Player | Fungus | Bat | Zombie | Goblin deriving (Eq)
data Creature = Creature
    { _location :: Coord
    , _c_glyph :: Char
    , _c_style :: StyleType
    , _c_id :: Int
    , _c_kind :: CreatureKind
    , _name :: String
    , _attack_power :: Int
    , _defense :: Int
    , _hp :: Int
    , _maxHp :: Int
    , _visionRadius :: Int
    , _inventory :: [Item]
    , _maxInv :: Int
    , _weapon :: Maybe Item
    , _armor :: Maybe Item
    , _food :: Int
    , _maxFood :: Int
    , _xp :: Int
    , _level :: Int
    , _levelUpgrades :: Int
    }
makeLenses ''Creature

instance Eq Creature where
    (==) a b = a^.c_id == b^.c_id

type CreatureState = State Creature

data TileKind = Floor | Wall | StairsUp | StairsDown | Unknown deriving (Eq, Show)

data Tile = Tile
    { _kind  :: TileKind
    , _glyph :: Char
    , _t_description :: String
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
    , _creatures :: M.Map Int Creature
    , _items :: M.Map Coord Item
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
    }
makeLenses ''Game

-- identical function signature:
-- player :: Functor f => (Creature -> f Creature) -> Game -> f Game
player :: Lens' Game Creature
player f game = 
    let updatePlayer game p' = (creatures %~ (M.insert 0 p')) game
    in fmap (updatePlayer game) (f (_creatures game M.! 0))

type GameState = State Game

nextInt :: GameState Int
nextInt = curId <+= 1

getStyle :: StyleType -> Game -> CursesStyle
getStyle styleType game = (game^.styles) M.! styleType

ui :: GameState Screen
ui = use $ uis.to last

splitBy :: Int -> [a] -> [[a]]
splitBy width [] = []
splitBy width xs = (take width xs) : (splitBy width (drop width xs))

data Climb = Up | Down deriving (Eq)

data Direction = N | E | S | W
               | NE | SE | SW | NW

offsetDir :: Direction -> Coord
offsetDir N = (0, -1, 0)
offsetDir E = (1,  0, 0)
offsetDir S = (0,  1, 0)
offsetDir W = (-1, 0, 0)
offsetDir NE = (offsetDir N) <+> (offsetDir E)
offsetDir SE = (offsetDir S) <+> (offsetDir E)
offsetDir SW = (offsetDir S) <+> (offsetDir W)
offsetDir NW = (offsetDir N) <+> (offsetDir W)

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
dropScreen = uis %= init

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

dropItemFilter _ = True
equipItemFilter i = i^.i_attackPower > 0 || i^.i_defensePower > 0
eatItemFilter i = i^.i_foodValue /= 0
examineItemFilter _ = True
throwItemFilter i = i^.i_throwAttackPower > 0
