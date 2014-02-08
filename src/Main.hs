{-# LANGUAGE TemplateHaskell #-}

module Main where

import Prelude hiding (floor)

import Control.Lens
import Control.Monad (replicateM_, replicateM, forM)
import Control.Monad.State.Strict (execState, get, runState)
import Data.Map.Strict as M (fromList, union)
import System.Console.ANSI
import System.IO (hGetChar, hGetEcho, hSetEcho, stdin)
import System.Random (getStdGen, randomR, randomRs, StdGen)

import Draw
import Game
import Input
import World

emptyUis :: Game -> Bool
emptyUis game = null $ game^.uis

createPlayer :: Coord -> Creature
createPlayer loc = Creature
    { _location = loc
    , _c_glyph = '@'
    , _c_color = Blue
    , _c_id = Just 0
    , _attack_power = 10
    , _hp = 40
    , _maxHp = 40 }

fungiPerLevel = 5

populateGame :: StdGen -> GameState ()
populateGame g = do
    game <- get
    let (fungi,g') = (runState $ forM [0..(gameDepth-1)] $ \depth -> replicateM fungiPerLevel $ createFungus depth game) g
        fungi' = concat fungi
    ids <- forM fungi' $ \_ -> nextInt
    let fungi'' = map (\(fungus,cid) -> (c_id .~ (Just cid)) fungus) (zip fungi' ids)
    creatures %= (union (M.fromList $ zip ids fungi''))

createFungus :: Int -> Game -> RandomState Creature
createFungus depth game = do
    loc <- findEmptyLocation depth game
    return $ Creature
        { _location = loc
        , _c_glyph = 'f'
        , _c_color = Green
        , _c_id = Nothing
        , _attack_power = 0
        , _hp = 1
        , _maxHp = 1 }

createGame :: IO Game
createGame = do
    level <- createLevel
    let world = (execState $ replicateM_ 8 smoothWorld) level
    g <- getStdGen
    let thePlayer = createPlayer (0, 0, 0)
        game = Game { _uis = [ Start ]
                    , _level = world
                    , _player = thePlayer
                    , _creatures = M.fromList []
                    , _curId = 0
                    }
        (playerLoc, g') = (runState $ findEmptyLocation 0 game) g
    let game' = (player.location .~ playerLoc) game
    return $ (execState $ do
        populateGame g'
        ) game'

gameLoop :: Game -> IO ()
gameLoop game = do
    clearScreen
    resetColor
    drawGame game
    ch <- getInput
    let game' = (execState $ processInput ch) game
    if emptyUis game'
       then return ()
       else gameLoop game'

main :: IO ()
main = do
    echo <- hGetEcho stdin
    hSetEcho stdin False
    hideCursor
    createGame >>= gameLoop
    setCursorPosition 0 0
    clearScreen
    showCursor
    hSetEcho stdin echo

