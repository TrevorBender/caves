{-# LANGUAGE TemplateHaskell #-}

module World where

import Prelude hiding (floor)

import Control.Lens
import Control.Monad.State.Strict (State, get, put, execState)
import Data.Array as A
import Data.Maybe (isNothing, isJust)
import Data.Map.Strict as M (elems)
import System.Random (getStdGen, randomR, randomRs, StdGen)

import Game

floor = Tile { _kind = Floor , _glyph = '.' }
wall = Tile { _kind = Wall , _glyph = '#' }
outOfBounds = Tile { _kind = Wall , _glyph = ' ' }
stairsDown = Tile { _kind = StairsDown , _glyph = '>' }
stairsUp = Tile { _kind = StairsUp , _glyph = '<' }
unknownTile = Tile { _kind = Unknown , _glyph = ' ' }

seeThrough :: Game -> Coord -> Bool
seeThrough game = (`elem` [Floor, StairsUp, StairsDown]) . (^.kind) . tileAt game

tileAt :: Game -> Coord -> Tile
tileAt game = tileAtWorld (game^.world)

tileAtWorld :: GameWorld -> Coord -> Tile
tileAtWorld world loc = if inBounds loc then world ! (reverseCoord loc) else outOfBounds

creatureAt :: Coord -> GameState (Maybe Creature)
creatureAt loc = do
    game <- get
    return $ case filter (\c -> c^.location == loc) (M.elems (game^.creatures)) of
         [] -> Nothing
         c:_ -> Just c

findEmptyLocation :: Int -> GameState Coord
findEmptyLocation depth = do
    game <- get
    let g = game^.stdGen
        (x, g') = randomR (0, gameWidth-1) g
        (y, g'') = randomR (0, gameHeight-1) g'
        loc = (fromIntegral x, fromIntegral y, depth)
    stdGen .= g''
    empty <- isEmpty loc
    if empty
       then return loc
       else findEmptyLocation depth

int2Tile :: Int -> Tile
int2Tile n = [floor, wall] !! n

gameBounds :: (Coord, Coord)
gameBounds = ((0,0,0), (gameDepth-1, gameHeight-1, gameWidth-1))

list2GameWorld :: [Tile] -> GameWorld
list2GameWorld = listArray gameBounds

randomWorld :: Int -> Int -> Int -> StdGen -> GameWorld
randomWorld width height depth g = list2GameWorld tiles
    where rs = take (width * height * depth) (randomRs (0, 1) g)
          tiles = map int2Tile rs

createWorld :: GameState ()
createWorld = do
    game <- get
    let g = game^.stdGen
    world .= randomWorld gameWidth gameHeight gameDepth g

smoothWorld :: GameState ()
smoothWorld = do
    game <- get
    let lvl = game^.world
        ixs = A.indices lvl
        tiles = map (newElem lvl) (map reverseCoord ixs)
        world' = list2GameWorld tiles
    world .= world'
    where newElem :: GameWorld -> Coord -> Tile
          newElem world ix = if floors >= walls then floor else wall
              where neighbors = (tileAtWorld world ix) : neighbors8 ix world
                    floors = length $ filter (== floor) neighbors
                    walls = (length neighbors) - floors

isFloor :: Coord -> GameState Bool
isFloor loc = do
    game <- get
    return $ (tileAt game loc) == floor

isCreature :: Coord -> GameState Bool
isCreature loc = do
    creature <- creatureAt loc
    return $ isJust creature

isEmpty :: Coord -> GameState Bool
isEmpty loc = do
    tileOK <- isFloor loc
    hasCreature <- isCreature loc
    return $ tileOK && (not hasCreature)
