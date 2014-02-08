module Generation where

import Control.Lens
import Control.Monad (replicateM_, replicateM, forM)
import Control.Monad.State.Strict (execState, get, runState)
import Data.Map.Strict as M (fromList, union)
import System.Console.ANSI
import System.Random (getStdGen, StdGen)

import Game
import World

createPlayer :: Creature
createPlayer = Creature
    { _location = (0,0,0) -- temporary location
    , _c_glyph = '@'
    , _c_color = Blue
    , _c_id = Just 0
    , _attack_power = 10
    , _hp = 40
    , _maxHp = 40 }

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

fungiPerLevel = 5

populateGame :: StdGen -> GameState ()
populateGame g = do
    game <- get
    let (fungi,g') = (runState $ forM [0..(gameDepth-1)] $ \depth -> replicateM fungiPerLevel $ createFungus depth game) g
        fungi' = concat fungi
    ids <- forM fungi' $ \_ -> nextInt
    let fungi'' = map (\(fungus,cid) -> (c_id .~ (Just cid)) fungus) (zip fungi' ids)
    creatures %= (union (M.fromList $ zip ids fungi''))

createGame :: IO Game
createGame = do
    level <- createLevel
    let world = (execState $ replicateM_ 8 smoothWorld) level
    g <- getStdGen
    let thePlayer = createPlayer
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
