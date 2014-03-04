module Generation
    ( createGame
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
import Creature (createPlayer, createFungus, createBat, createZombie)

fungiPerLevel = 5
batsPerLevel = 5

populateGame :: GameState ()
populateGame = do
    populateCreature createFungus (\_ -> fungiPerLevel)
    populateCreature createBat (\_ -> batsPerLevel)
    populateCreature createZombie id

    where populateCreature :: (Int -> GameState Creature) -> (Int -> Int) -> GameState ()
          populateCreature createCreature cPerLevel = do
              cs <- forM [0..(gameDepth-1)] $ \depth -> replicateM (cPerLevel depth) $ createCreature depth
              let cMap = M.fromList $ map (\c -> (c^.c_id, c)) (concat cs)
              creatures %= (M.union cMap)

createVictoryStairs :: GameState ()
createVictoryStairs = do
    loc <- findEmptyLocation 0
    empty <- isEmpty loc
    if empty
       then world %= (//[(reverseCoord loc, stairsUp)])
       else createVictoryStairs

createStairs :: GameState ()
createStairs = do
    (_,rMap,nMap) <- use regionMap
    let rNums = M.keys $ M.filter (\locs -> any (\(_,_,z) -> z < gameDepth - 1) locs) nMap
    forM_ rNums connectRegionsDown

connectRegionsDown :: Int -> GameState ()
connectRegionsDown num = do
    (_,rMap,nMap) <- use regionMap
    let (_,_,depth) = head $ nMap ! num
        rBelow = M.keys $ M.filter (\locs -> any (\(_,_,z) -> z == depth + 1) locs) nMap
    forM_ rBelow (connectRegionDown depth num)

connectRegionDown :: Int -> Int -> Int -> GameState ()
connectRegionDown depth r1 r2 = do
    (_,_,nMap) <- use regionMap
    let adjusted = map (\(x,y,z) -> (x,y,z-1)) (nMap ! r2)
        overlap = intersect (nMap ! r1) adjusted
    unless (null overlap) $ replicateM_ (ceiling $ fromIntegral (length overlap) / 250) $ do
        loc <- randomL overlap
        createStairDown loc

createStairDown :: Coord -> GameState ()
createStairDown loc = do
    let lowerLoc = loc <+> offsetClimb Down
    world %= (//[(reverseCoord loc, stairsDown), (reverseCoord lowerLoc, stairsUp)])
    return ()

item :: Item -> Int -> GameState Item
item item depth = do
    loc <- findEmptyLocation depth
    id <- nextInt
    return item { _i_location = loc
                , _i_id = id
                }

defaultItem = Item { _i_location = (0,0,0)
                   , _i_id = -1
                   , _i_name = "<fixme: default>"
                   , _i_style = DefaultStyle
                   , _i_glyph = 'X'
                   , _i_attackPower = 0
                   , _i_defensePower = 0
                   , _i_foodValue = 0
                   , _i_throwAttackPower = 0
                   }

createRock :: Int -> GameState Item
createRock = item defaultItem
    { _i_name = "rock"
    , _i_glyph = ','
    , _i_throwAttackPower = 1
    }

createVictoryItem :: GameState Item
createVictoryItem = item defaultItem
    { _i_name = "idol"
    , _i_style = VictoryItemStyle
    , _i_glyph = '*'
    } (gameDepth-1)

createDagger :: Int -> GameState Item
createDagger = item defaultItem
    { _i_name = "dagger"
    , _i_glyph = ')'
    , _i_attackPower = 5
    , _i_throwAttackPower = 5
    }

createSword :: Int -> GameState Item
createSword = item defaultItem
    { _i_name = "sword"
    , _i_style = SwordStyle
    , _i_glyph = ')'
    , _i_attackPower = 10
    , _i_throwAttackPower = 10
    }

createStaff :: Int -> GameState Item
createStaff = item defaultItem
    { _i_name = "staff"
    , _i_style = StaffStyle
    , _i_glyph = ')'
    , _i_attackPower = 5
    , _i_defensePower = 5
    , _i_throwAttackPower = 5
    }

createTunic :: Int -> GameState Item
createTunic = item defaultItem
    { _i_name = "tunic"
    , _i_style = StaffStyle
    , _i_glyph = '['
    , _i_defensePower = 2
    }

createChainmail :: Int -> GameState Item
createChainmail = item defaultItem
    { _i_name = "chainmail"
    , _i_style = SwordStyle
    , _i_glyph = '['
    , _i_defensePower = 4
    }

createPlatemail :: Int -> GameState Item
createPlatemail = item defaultItem
    { _i_name = "platemail"
    , _i_glyph = '['
    , _i_defensePower = 4
    }

createBread :: Int -> GameState Item
createBread = item defaultItem
    { _i_name = "bread"
    , _i_glyph = '8'
    , _i_foodValue = 200
    }

randomWeapon :: Int -> GameState Item
randomWeapon depth = do
    cf <- randomL [ createDagger, createSword, createStaff ]
    cf depth

randomArmor :: Int -> GameState Item
randomArmor depth = do
    cf <- randomL [ createTunic, createChainmail, createPlatemail ]
    cf depth

createItems :: GameState ()
createItems = do
    createItem createRock (\_ -> div (gameWidth * gameHeight) 50)
    createItem randomWeapon (\_ -> 2)
    createItem randomArmor (\_ -> 2)
    createItem createBread (\_ -> 1)

    victoryItem <- createVictoryItem
    items %= M.insert (victoryItem^.i_location) victoryItem

    where createItem :: (Int -> GameState Item) -> (Int -> Int) -> GameState ()
          createItem cf nf = do
              iss <- forM [0..(gameDepth-1)] $ \depth -> replicateM (nf depth) (cf depth)
              let im = M.fromList $ map (\i -> (i^.i_location, i)) (concat iss)
              items %= M.union im

cleanUp = regionMap .= emptyRegionMap

updateNewGame :: Game -> Game
updateNewGame = execState $ do
    createWorld
    replicateM_ 8 smoothWorld
    createRegionMap
    removeSmallRegions
    createStairs
    createVictoryStairs
    loc <- findEmptyLocation 0
    player.location .= loc
    targetLoc .= loc
    populateGame
    createItems
    cleanUp

fillRegion :: Coord                   -- ^ Starting location
           -> Int                     -- ^ Region number
           -> GameWorld               -- ^ 3D game world
           -> RegionMap               -- ^ resulting region map and size
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
    (_,rMap,nMap) <- use regionMap
    let smallRegions = M.filter (\xs -> length xs < 25) nMap
    forM_ (M.keys smallRegions) removeSmallRegion

deleteAll :: [Coord] -> Map Coord (Maybe Int) -> Map Coord (Maybe Int)
deleteAll ks = mapWithKey (\k x -> if elem k ks then Nothing else x)

removeSmallRegion :: Int -> GameState ()
removeSmallRegion num = do
    (rNum,rMap,nMap) <- use regionMap
    let locs = nMap ! num
        assocList = map (\loc -> (reverseCoord loc, wall)) locs
    world %= (// assocList)
    regionMap .= (rNum, deleteAll locs rMap, delete num nMap)
    return ()

createRegionMap :: GameState ()
createRegionMap = do
    w <- use world
    let rMap = foldr (toRegionMap w) (0,M.empty,M.empty) (A.assocs w)
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
        game = Game { _uis = [ Play, Start ]
                    , _world = emptyWorld
                    , _visibleWorld = unknownWorld
                    , _creatures = M.fromList [(0,thePlayer)]
                    , _items = M.empty
                    , _messages = []
                    , _curId = 0
                    , _stdGen = g
                    , _updated = False
                    , _window = win
                    , _styles = cstyles
                    , _regionMap = emptyRegionMap
                    , _targetLoc = (0,0,0)
                    }
    return $ updateNewGame game

