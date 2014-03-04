{-# LANGUAGE TemplateHaskell #-}

module World where

import Prelude hiding (floor)

import Control.Lens
import Control.Monad.State.Strict (State, get, put, execState)
import Data.Array as A
import Data.Maybe (isNothing, isJust, fromJust)
import Data.Map.Strict as M (elems, lookup, delete, (!), member)
import System.Random (randomRs, StdGen)

import Game
import Line (line)
import Random as R (randomR)

floor = Tile { _kind = Floor , _glyph = '.' , _t_description = "cave floor" }
wall = Tile { _kind = Wall , _glyph = '#' , _t_description = "cave wall" }
outOfBounds = Tile { _kind = Wall , _glyph = ' ' , _t_description = "out of bounds" }
stairsDown = Tile { _kind = StairsDown , _glyph = '>' , _t_description = "stairs down" }
stairsUp = Tile { _kind = StairsUp , _glyph = '<' , _t_description = "stairs up" }
unknownTile = Tile { _kind = Unknown , _glyph = ' ' , _t_description = "unknown" }

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

canSee' :: Coord -> Creature -> GameState Bool
canSee' loc c = do
    game <- get
    return $ canSee game loc c

updateVisibleTiles :: GameState ()
updateVisibleTiles = do
    game <- get
    let (_,_,z) = game^.player.location
        visibleIds = filter (\loc -> canSee game loc (game^.player)) [(x,y,z) | z <- [0..(gameDepth-1)], y <- [0..(gameHeight-1)], x <- [0..(gameWidth-1)]]
        updates = map (\loc -> (reverseCoord loc, tileAt game loc)) visibleIds
    visibleWorld %= (// updates)

visibleTileAt :: Game -> Coord -> Tile
visibleTileAt game loc = if inBounds loc then (game^.visibleWorld) A.! (reverseCoord loc) else outOfBounds

seeThrough :: Game -> Coord -> Bool
seeThrough game = (`elem` [Floor, StairsUp, StairsDown]) . (^.kind) . tileAt game

tileAt' :: Coord -> GameState Tile
tileAt' loc = do
    w <- use world
    return $ tileAtWorld w loc

tileAt :: Game -> Coord -> Tile
tileAt game = tileAtWorld (game^.world)

tileAtWorld :: GameWorld -> Coord -> Tile
tileAtWorld world loc = if inBounds loc then world A.! (reverseCoord loc) else outOfBounds

creatureAt :: Coord -> GameState (Maybe Creature)
creatureAt loc = do
    cs <- use $ creatures .to M.elems
    return $ case filter (\c -> c^.location == loc) cs of
         [] -> Nothing
         c:_ -> Just c

findEmptyLocation :: Int -> GameState Coord
findEmptyLocation depth = do
    x <- randomR (0, gameWidth-1)
    y <- randomR (0, gameHeight-1)
    let loc = (x, y, depth)
    empty <- isEmpty loc
    if empty
       then return loc
       else findEmptyLocation depth


gameBounds :: (Coord, Coord)
gameBounds = ((0,0,0), (gameDepth-1, gameHeight-1, gameWidth-1))

list2GameWorld :: [Tile] -> GameWorld
list2GameWorld = listArray gameBounds

randomWorld :: GameState GameWorld
randomWorld = do
    g <- use stdGen
    let rs = take (gameWidth * gameHeight * gameDepth) (randomRs (0, 1) g)
        tiles = map int2Tile rs
    return $ list2GameWorld tiles

    where int2Tile :: Int -> Tile
          int2Tile n = [floor, wall] !! n

createWorld :: GameState ()
createWorld = (world .=) =<< randomWorld

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
    tile <- tileAt' loc
    return $ tile == floor

isCreature :: Coord -> GameState Bool
isCreature loc = do
    creature <- creatureAt loc
    return $ isJust creature

itemAt :: Coord -> GameState Item
itemAt loc = use $ items .to (M.! loc)

isItem :: Coord -> GameState Bool
isItem loc = use $ items .to (member loc)

isEmpty :: Coord -> GameState Bool
isEmpty loc = do
    tileOK <- isFloor loc
    hasCreature <- isCreature loc
    hasItem <- isItem loc
    return $ tileOK && (not hasCreature) && not hasItem

removeItemFromWorld :: Coord -> GameState ()
removeItemFromWorld loc = items %= (delete loc)

describe :: Coord -> GameState String
describe loc = do
    isC <- isCreature loc
    isI <- isItem loc
    vw <- use visibleWorld
    let t = tileAtWorld vw loc
    if t == unknownTile then return "Unknown"
    else if isC then do
        Just c <- creatureAt loc
        return $ "It's a " ++ (c^.name)
    else if isI then do
        i <- itemAt loc
        return $ describeItem i
    else do
        return $ "It's a " ++ t^.t_description

describeItem :: Item -> String
describeItem item = "It's a " ++ item^.i_name ++ ". " ++ itemDetails item
    where itemDetails item =
              let showIfValue i s = if i /= 0 then s ++ show i else ""
                  a = showIfValue (item^.i_attackPower) " attack: "
                  d = showIfValue (item^.i_defensePower) " defense: "
                  f = showIfValue (item^.i_foodValue) " food: "
                  in a ++ d ++ f
