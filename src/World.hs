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
import Line (line)

floor = Tile { _kind = Floor , _glyph = '.' }
wall = Tile { _kind = Wall , _glyph = '#' }
outOfBounds = Tile { _kind = Wall , _glyph = ' ' }
stairsDown = Tile { _kind = StairsDown , _glyph = '>' }
stairsUp = Tile { _kind = StairsUp , _glyph = '<' }
unknownTile = Tile { _kind = Unknown , _glyph = ' ' }

canSee :: Game -> Coord -> Creature -> Bool
canSee game loc@(x,y,z) c =
    let (cx,cy,cz) = c^.location
        sameDepth = z == cz
        distance = (cx - x) * (cx - x) + (cy - y) * (cy - y)
        inVision = distance <= (c^.visionRadius) * (c^.visionRadius)
        ln = line (cx, cy) (x, y)
        threeD2D (x, y) = (x, y, z)
        notBlocked = (<= 1) $ length $ dropWhile (seeThrough game . threeD2D) ln
    in sameDepth && inVision && notBlocked

updateVisibleTiles :: GameState ()
updateVisibleTiles = do
    game <- get
    let (_,_,z) = game^.player.location
        visibleIds = filter (\loc -> canSee game loc (game^.player)) [(x,y,z) | z <- [0..(gameDepth-1)], y <- [0..(gameHeight-1)], x <- [0..(gameWidth-1)]]
        updates = map (\loc -> (reverseCoord loc, tileAt game loc)) visibleIds
    visibleWorld %= (// updates)

visibleTileAt :: Game -> Coord -> Tile
visibleTileAt game loc = if inBounds loc then (game^.visibleWorld) ! (reverseCoord loc) else outOfBounds

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
