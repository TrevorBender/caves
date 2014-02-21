{-# LANGUAGE TemplateHaskell #-}

module Game where

import Control.Lens
import Control.Monad (when)
import Control.Monad.State.Strict (State, get)
import Data.Array as A
import Data.Map.Strict as M (Map, (!), insert)
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

data Screen = Start | Win | Lose | Play

type Coord = (Int, Int, Int)

data StyleType = DefaultStyle
               | PlayerStyle
               | FungusStyle
               | OutOfSiteStyle
               | BatStyle
               deriving (Eq, Ord)

data CreatureKind = Player | Fungus | Bat
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
    }
makeLenses ''Creature

instance Eq Creature where
    (==) a b = a^.c_id == b^.c_id

data TileKind = Floor | Wall | StairsUp | StairsDown | Unknown deriving (Eq)

data Tile = Tile
    { _kind  :: TileKind
    , _glyph :: Char
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
    , _messages :: [String]

    , _curId :: Int     -- for id generation
    , _stdGen :: StdGen -- for random number generation

    -- region map generation
    , _regionMap :: RegionMap
    , _drawRegions :: Bool

    , _window :: Window
    , _styles :: Map StyleType CursesStyle
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
nextInt = do
    game <- get
    let cur = game^.curId
        next = cur + 1
    curId .= next
    return next

getStyle :: StyleType -> Game -> CursesStyle
getStyle styleType game = (game^.styles) M.! styleType

ui :: Game -> Screen
ui game = head $ game^.uis

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
    game <- get
    let ploc = game^.player.location
        vision = game^.player.visionRadius
        inRange = sameDepth loc ploc && distanceSq loc ploc <= vision * vision
    when inRange $ messages %= (s:)

lose :: GameState ()
lose = uis .= [Lose]

quit :: GameState ()
quit = uis .= []

win :: GameState ()
win = uis .= [Win]

debug :: String -> a -> a
debug str x = if debugOn then debug' str x else x
    where debug' str x = unsafePerformIO $ do
              hPutStrLn stderr str
              return x
