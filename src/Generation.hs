module Generation
    ( createGame
    , createFungus
    ) where

import Prelude as P hiding (floor)
import Control.Lens
import Control.Monad (replicateM_, replicateM, forM, forM_, foldM, unless)
import Control.Monad.State.Strict (get, put, modify, execState, runState, State)
import Data.Array as A (listArray, (//), assocs)
import Data.List as L (intersect)
import Data.Map.Strict as M
    ( Map, empty, fromList, union
    , insert, notMember, member, foldrWithKey
    , alter, filter, keys, (!), filterWithKey, delete, mapWithKey)
import Data.Maybe (fromJust)
import System.Random as R (getStdGen)
import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper

import Game
import World
import Random

createPlayer :: Creature
createPlayer = Creature
    { _location = (0,0,0) -- temporary location
    , _c_kind = Player
    , _c_glyph = '@'
    , _c_style = PlayerStyle
    , _c_id = 0
    , _name = "<fixme: you>"
    , _attack_power = 10
    , _defense = 3
    , _hp = 40
    , _maxHp = 40
    , _visionRadius = 20
    }

createFungus :: Int -> GameState Creature
createFungus depth = do
    loc <- findEmptyLocation depth
    thisId <- nextInt
    return $ Creature
        { _location = loc
        , _c_kind = Fungus
        , _c_glyph = 'f'
        , _c_style = FungusStyle
        , _c_id = thisId
        , _name = "lichen"
        , _attack_power = 0
        , _defense = 1
        , _hp = 1
        , _maxHp = 1
        , _visionRadius = 0
        }
fungiPerLevel = 5

createBat :: Int -> GameState Creature
createBat depth = do
    loc <- findEmptyLocation depth
    thisId <- nextInt
    return $ Creature
        { _location = loc
        , _c_kind = Bat
        , _c_glyph = 'b'
        , _c_style = BatStyle
        , _c_id = thisId
        , _name = "bat"
        , _attack_power = 4
        , _defense = 4
        , _hp = 5
        , _maxHp = 5
        , _visionRadius = 0
        }
batsPerLevel = 5

populateCreature :: (Int -> GameState Creature) -> (Int -> Int) -> GameState ()
populateCreature createCreature cPerLevel = do
    cs <- forM [0..(gameDepth-1)] $ \depth -> replicateM (cPerLevel depth) $ createCreature depth
    let cMap = M.fromList $ map (\c -> (c^.c_id, c)) (concat cs)
    creatures %= (M.union cMap)

populateGame :: GameState ()
populateGame = do
    populateCreature createFungus (\_ -> fungiPerLevel)
    populateCreature createBat (\_ -> batsPerLevel)

createVictoryStairs :: GameState ()
createVictoryStairs = do
    loc <- findEmptyLocation 0
    empty <- isEmpty loc
    if empty
       then world %= (//[(reverseCoord loc, stairsUp)])
       else createVictoryStairs

createStairs :: GameState ()
createStairs = do
    game <- get
    let (_,rMap,nMap) = game^.regionMap
        rNums = M.keys $ M.filter (\locs -> any (\(_,_,z) -> z < gameDepth - 1) locs) nMap
    forM_ rNums connectRegionsDown

connectRegionsDown :: Int -> GameState ()
connectRegionsDown num = do
    game <- get
    let (_,rMap,nMap) = game^.regionMap
        (_,_,depth) = head $ nMap ! num
        rBelow = M.keys $ M.filter (\locs -> any (\(_,_,z) -> z == depth + 1) locs) nMap
    forM_ rBelow (connectRegionDown depth num)

connectRegionDown :: Int -> Int -> Int -> GameState ()
connectRegionDown depth r1 r2 = do
    game <- get
    let (_,_,nMap) = game^.regionMap
        adjusted = map (\(x,y,z) -> (x,y,z-1)) (nMap ! r2)
        overlap = intersect (nMap ! r1) adjusted
    unless (null overlap) $ replicateM_ (ceiling $ fromIntegral (length overlap) / 250) $ do
        loc <- randomL overlap
        createStairDown loc

createStairDown :: Coord -> GameState ()
createStairDown loc = do
    let lowerLoc = loc <+> offsetClimb Down
    world %= (//[(reverseCoord loc, stairsDown), (reverseCoord lowerLoc, stairsUp)])
    return ()

createRock :: Int -> GameState Item
createRock depth = do
    loc <- findEmptyLocation depth
    return $ Item { _i_location = loc
                  , _i_name = "rock"
                  , _i_style = DefaultStyle
                  , _i_glyph = ','
                  }

createItems :: GameState ()
createItems = do
    rocks <- forM [0..(gameDepth-1)] $ \depth -> replicateM (div (gameWidth * gameHeight) 50) $ createRock depth
    let itemMap = M.fromList $ map (\i -> (i^.i_location, i)) (concat rocks)
    items %= (M.union itemMap)

cleanUp = regionMap .= emptyRegionMap

updateNewGame :: Game -> Game
updateNewGame = execState $ do
    createWorld
    replicateM_ 8 smoothWorld
    createRegionMap
    removeSmallRegions
    createStairs
    createVictoryStairs
    findEmptyLocation 0 >>= (player.location .=)
    populateGame
    createItems
    cleanUp

fillRegion :: Coord                   -- ^ Starting location
           -> Int                     -- ^ Region number
           -> GameWorld               -- ^ 3D game world
           -> RegionMap        -- ^ resulting region map and size
fillRegion loc num world = execState (dfs' loc world) (num, M.empty, M.empty)
    where dfs' :: Coord -> GameWorld -> State RegionMap ()
          dfs' loc world = do
              (num, regionMap, nMap) <- get
              let neighbors = neighborsCoords loc
                  neighbors' = P.filter (\loc -> inBounds loc && (tileAtWorld world loc)^.kind == Floor) neighbors
                  newNeighbors = P.filter (\loc -> notMember loc regionMap) neighbors'
                  updateNumMap loc (Just xs) = Just$ loc:xs
                  updateNumMap loc Nothing = Just [loc]
              forM_ newNeighbors $ \loc -> modify $ \(num, rMap, nMap) -> (num, insert loc (Just $ num) rMap, M.alter (updateNumMap loc) num nMap)
              forM_ newNeighbors $ \loc -> dfs' loc world

removeSmallRegions :: GameState ()
removeSmallRegions = do
    game <- get
    let (_,rMap,nMap) = game^.regionMap
        smallRegions = M.filter (\xs -> length xs < 25) nMap
    forM_ (M.keys smallRegions) removeSmallRegion

deleteAll :: [Coord] -> Map Coord (Maybe Int) -> Map Coord (Maybe Int)
deleteAll ks = mapWithKey (\k x -> if elem k ks then Nothing else x)

removeSmallRegion :: Int -> GameState ()
removeSmallRegion num = do
    game <- get
    let (rNum,rMap,nMap) = game^.regionMap
        locs = nMap ! num
        assocList = map (\loc -> (reverseCoord loc, wall)) locs
    world %= (// assocList)
    regionMap .= (rNum, deleteAll locs rMap, delete num nMap)
    return ()

createRegionMap :: GameState ()
createRegionMap = do
    game <- get
    let rMap = foldr (toRegionMap $ game^.world) (0,M.empty,M.empty) (A.assocs $ game^.world)
    regionMap .= rMap

    where toRegionMap :: GameWorld -> (Coord, Tile) -> RegionMap -> RegionMap
          toRegionMap world (loc, tile) (num,rMap,nMap) =
              case tile^.kind of
                   Floor -> handleFloor world (reverseCoord loc) num rMap nMap
                   _ -> (num, insert (reverseCoord loc) Nothing rMap, nMap)

          handleFloor world loc num rMap nMap = if member loc rMap
                                                   then (num, rMap, nMap)
                                                   else let (_,rMap',nMap') = fillRegion loc num world
                                                            in (num+1, M.union rMap rMap', M.union nMap nMap')

-- should this be a Maybe
emptyRegionMap = (0, M.empty, M.empty)

createGame :: Window -> Map StyleType CursesStyle -> IO Game
createGame win cstyles = do
    g <- getStdGen
    let thePlayer = createPlayer
        emptyWorld = A.listArray ((0,0,0), (0,0,0)) [floor]
        unknownWorld = A.listArray gameBounds (repeat unknownTile)
        game = Game { _uis = [ Start ]
                    , _world = emptyWorld
                    , _visibleWorld = unknownWorld
                    , _creatures = M.fromList [(0,thePlayer)]
                    , _items = M.empty
                    , _messages = []
                    , _curId = 0
                    , _stdGen = g
                    , _window = win
                    , _styles = cstyles
                    , _regionMap = emptyRegionMap
                    }
    return $ updateNewGame game

