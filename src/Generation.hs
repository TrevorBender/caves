module Generation
    ( createGame
    , createFungus
    ) where

import Prelude hiding (floor)

import Control.Lens
import Control.Monad (replicateM_, replicateM, forM, forM_)
import Control.Monad.State.Strict (execState, runState)
import Data.Array as A (listArray, (//))
import Data.Map.Strict as M (fromList, union, insert)
import Data.Maybe (fromJust)
import System.Random as R (getStdGen)
import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper

import Game
import World

createPlayer :: Creature
createPlayer = Creature
    { _location = (0,0,0) -- temporary location
    , _c_kind = Player
    , _c_glyph = '@'
    , _c_style = 1
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
        , _c_style = 2
        , _c_id = thisId
        , _name = "lichen"
        , _attack_power = 0
        , _defense = 1
        , _hp = 1
        , _maxHp = 1
        , _visionRadius = 0
        }

fungiPerLevel = 5

populateGame :: GameState ()
populateGame = do
    fungi <- forM [0..(gameDepth-1)] $ \depth -> replicateM fungiPerLevel $ createFungus depth
    let fungiMap = M.fromList $ map (\fungus -> (fungus^.c_id, fungus)) (concat fungi)
    creatures %= (union fungiMap)

createVictoryStairs :: GameState ()
createVictoryStairs = do
    loc <- findEmptyLocation 0
    empty <- isEmpty loc
    if empty
       then world %= (//[(reverseCoord loc, stairsUp)])
       else createVictoryStairs

createStairs :: GameState ()
createStairs = do
    forM_ [0..(gameDepth - 2)] $ \depth -> do
        createStairDown depth
    createVictoryStairs

createStairDown :: Int -> GameState ()
createStairDown depth = do
    loc <- findEmptyLocation depth
    let lowerLoc = loc <+> offsetClimb Down
    thisEmpty <- isEmpty loc
    lowerEmpty <- isEmpty lowerLoc
    if thisEmpty && lowerEmpty
       then do
           world %= (//[(reverseCoord loc, stairsDown)])
           world %= (//[(reverseCoord lowerLoc, stairsUp)])
       else createStairDown depth


updateNewGame :: Game -> Game
updateNewGame = execState $ do
    createWorld
    replicateM_ 8 smoothWorld
    createStairs
    findEmptyLocation 0 >>= (player.location .=)
    populateGame

createGame :: Window -> [CursesStyle] -> IO Game
createGame win cstyles = do
    g <- getStdGen
    let thePlayer = createPlayer
        emptyWorld = A.listArray ((0,0,0), (0,0,0)) [floor]
        unknownWorld = A.listArray gameBounds (repeat unknownTile)
        game = Game { _uis = [ Start ]
                    , _world = emptyWorld
                    , _visibleWorld = unknownWorld
                    , _player = thePlayer
                    , _creatures = M.fromList []
                    , _messages = []
                    , _curId = 0
                    , _stdGen = g
                    , _window = win
                    , _styles = cstyles
                    }
    return $ updateNewGame game

