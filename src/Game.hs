{-# LANGUAGE TemplateHaskell #-}

module Game where

import Control.Lens
import Control.Monad.State.Strict (State, get)
import Data.Array
import Data.Map.Strict as M (Map)
import System.Console.ANSI
import System.Random (StdGen)

gameWidth, gameHeight, gameDepth :: Int
gameWidth = 80
gameHeight = 30
gameDepth = 2

data Screen = Start | Win | Lose | Play deriving (Show)

type Coord = (Int, Int, Int)

data CreatureKind = Player | Fungus deriving (Show)
data Creature = Creature
    { _location :: Coord
    , _c_glyph :: Char
    , _c_color :: Color
    , _c_id :: Int
    , _attack_power :: Int
    , _hp :: Int
    , _maxHp :: Int
    , _c_kind :: CreatureKind
    } deriving (Show)
makeLenses ''Creature

data TileKind = Floor | Wall deriving (Show, Eq)

data Tile = Tile
    { _kind  :: TileKind
    , _glyph :: Char
    } deriving (Show)
makeLenses ''Tile

instance Eq Tile where
    -- (==) :: Tile -> Tile -> Bool
    (==) a b = a^.kind == b^.kind

type GameWorld = Array Coord Tile

data Game = Game
    { _uis   :: [Screen]
    , _world :: GameWorld
    , _player :: Creature
    , _curId :: Int
    , _stdGen :: StdGen
    , _creatures :: M.Map Int Creature
    } deriving (Show)
makeLenses ''Game

type GameState = State Game

nextInt :: GameState Int
nextInt = do
    game <- get
    let cur = game^.curId
        next = cur + 1
    curId .= next
    return next

ui :: Game -> Screen
ui game = head $ game^.uis

splitBy :: Int -> [a] -> [[a]]
splitBy width [] = []
splitBy width xs = (take width xs) : (splitBy width (drop width xs))

data Climb = Up | Down

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

neighbors8 :: Coord -> GameWorld -> [Tile]
neighbors8 origin world = tiles
    where dirs = [ N, E, S, W, NE, SE, SW, NW ]
          offsets = map offsetDir dirs
          ixs = map (origin <+>) offsets
          ixs' = filter inBounds ixs
          tileAt = (world !) . reverseCoord
          tiles = map tileAt ixs'
