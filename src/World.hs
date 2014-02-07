{-# LANGUAGE TemplateHaskell #-}

module World where

import Prelude hiding (floor)

import Control.Lens
import Data.Array
import System.Random (getStdGen, randomR, randomRs, RandomGen(..))

import Game

floor = Tile { _kind = Floor , _glyph = '.' }
wall = Tile { _kind = Wall , _glyph = '#' }

tileAt :: Game -> Coord -> Tile
tileAt game (x, y, z) = (game^.level) ! (z, y, x)

findEmptyLocation :: RandomGen g => g -> Int -> Game -> Coord
findEmptyLocation g depth game =
    if (tileAt game (fromIntegral x, fromIntegral y, depth)) == floor
       then (fromIntegral x, fromIntegral y, depth)
       else findEmptyLocation g'' depth game
   where (x, g') = randomR (0, gameWidth-1) g
         (y, g'') = randomR (0, gameHeight-1) g'

int2Tile :: Int -> Tile
int2Tile n = [floor, wall] !! n

list2GameLevel :: [Tile] -> GameLevel
list2GameLevel = listArray ((0,0,0), (gameDepth-1,gameHeight-1,gameWidth-1))

randomLevel :: RandomGen g => Int -> Int -> Int -> g -> GameLevel
randomLevel width height depth g = list2GameLevel tiles
    where rs = take (width * height * depth) (randomRs (0, 1) g)
          tiles = map int2Tile rs

createLevel :: IO GameLevel
createLevel = do
    g <- getStdGen
    return $ randomLevel gameWidth gameHeight gameDepth g
