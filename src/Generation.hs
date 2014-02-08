module Generation where

import Prelude hiding (floor)

import Control.Lens
import Control.Monad (replicateM_, replicateM, forM, when)
import Control.Monad.State.Strict (execState, get, runState)
import Data.Array as A (listArray)
import Data.Map.Strict as M (fromList, union, insert)
import Data.Maybe (isJust)
import System.Console.ANSI
import System.Random as R (getStdGen)

import Game
import World
import Random

createPlayer :: Creature
createPlayer = Creature
    { _location = (0,0,0) -- temporary location
    , _c_kind = Player
    , _c_glyph = '@'
    , _c_color = Blue
    , _c_id = 0
    , _attack_power = 10
    , _hp = 40
    , _maxHp = 40 }

createFungus :: Int -> GameState Creature
createFungus depth = do
    game <- get
    loc <- findEmptyLocation depth
    thisId <- nextInt
    return $ Creature
        { _location = loc
        , _c_kind = Fungus
        , _c_glyph = 'f'
        , _c_color = Green
        , _c_id = thisId
        , _attack_power = 0
        , _hp = 1
        , _maxHp = 1 }

fungiPerLevel = 5

populateGame :: GameState ()
populateGame = do
    game <- get
    fungi <- forM [0..(gameDepth-1)] $ \depth -> replicateM fungiPerLevel $ createFungus depth
    let fungiMap = M.fromList $ map (\fungus -> (fungus^.c_id, fungus)) (concat fungi)
    creatures %= (union fungiMap)

createGame :: IO Game
createGame = do
    g <- getStdGen
    let thePlayer = createPlayer
        game = Game { _uis = [ Start ]
                    , _world = A.listArray ((0,0,0),(0,0,0)) []
                    , _player = thePlayer
                    , _creatures = M.fromList []
                    , _curId = 0
                    , _stdGen = g
                    }
    return $ (execState $ do
        createWorld
        replicateM_ 8 smoothWorld
        findEmptyLocation 0 >>= (player.location .=)
        populateGame
        ) game

creatureTick :: Creature -> GameState ()
creatureTick c = creatureTick' (c^.c_kind) c

creatureTick' :: CreatureKind -> Creature -> GameState ()

creatureTick' Fungus c = do
    game <- get
    let loc@(_,_,depth) = c^.location
    r <- randomR (0,99)
    when (r == 99) $ do
        dir <- randomL [N,E,S,W,NE,SE,SW,NW]
        let offset = offsetDir dir
            loc' = offset <+> loc
        when (inBounds loc') $ do
            isFloor <- isFloor loc'
            isCreature <- isCreature loc'
            when (isFloor && not isCreature) $ do
                fungus <- createFungus depth
                let fungus' = (location .~ loc') fungus
                creatures %= (insert (fungus^.c_id) fungus')

creatureTick' _ _ = return ()

isFloor :: Coord -> GameState Bool
isFloor loc = do
    game <- get
    return $ (tileAt game loc) == floor

isCreature :: Coord -> GameState Bool
isCreature loc = do
    game <- get
    return $ isJust $ creatureAt game loc
