{-# LANGUAGE TemplateHaskell #-}

module Game where

import Control.Lens
import Data.Map.Strict as M
import System.Console.ANSI

gameWidth, gameHeight :: Int
gameWidth = 80
gameHeight = 30

data Screen = Start | Win | Lose | Play deriving (Show)

type Coord = (Int, Int)

data Creature = Creature
    { _location :: Coord
    , _c_glyph :: Char
    , _c_color :: Color
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

{-type Row = [Tile]-}
{-type GameLevel = [Row]-}

type Row = M.Map Int Tile
type GameLevel = M.Map Int Row

data Game = Game
    { _uis   :: [Screen]
    , _level :: GameLevel
    , _player :: Creature
    } deriving (Show)
makeLenses ''Game

ui :: Game -> Screen
ui game = head $ game^.uis
