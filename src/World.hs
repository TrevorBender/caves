{-# LANGUAGE TemplateHaskell #-}

module World where

import Prelude hiding (floor)

import Control.Lens
import System.Random (getStdGen, randomR, randomRs, RandomGen(..))

import Game

floor = Tile { _kind = Floor , _glyph = '.' }
wall = Tile { _kind = Wall , _glyph = '#' }

tileAt :: Game -> Coord -> Tile
tileAt game (x, y) = ((game^.level) !! y) !! x

findEmptyLocation :: RandomGen g => g -> Game -> Coord
findEmptyLocation g game =
    if (tileAt game (fromIntegral x, fromIntegral y)) == floor
       then (fromIntegral x, fromIntegral y)
       else findEmptyLocation g'' game
   where (x, g') = randomR (0, gameWidth-1) g
         (y, g'') = randomR (0, gameHeight-1) g'

int2Tile :: Int -> Tile
int2Tile n = [floor, wall] !! n

splitBy :: Int -> [a] -> [[a]]
splitBy width [] = []
splitBy width xs = (take width xs) : (splitBy width (drop width xs))

randomLevel :: RandomGen g => Int -> Int -> g -> GameLevel
randomLevel width height g = splitBy width tiles
    where rs = take (width * height) (randomRs (0, 1) g)
          tiles = map int2Tile rs

createLevel :: IO GameLevel
createLevel = do
    g <- getStdGen
    return $ randomLevel gameWidth gameHeight g
