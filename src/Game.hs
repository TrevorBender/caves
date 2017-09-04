module Game
    ( player
    , gameWidth, gameHeight, gameDepth
    , reverseCoord
    , inBounds
    , minBound
    , neighbors8
    , creature
    , effectDone
    , offsetDir
    , nextInt
    , lose
    , notify
    , pushScreen
    , (<+>)
    , quit
    , dropItemFilter
    , equipItemFilter
    , eatItemFilter
    , examineItemFilter
    , throwItemFilter
    , readItemFilter
    , quaffItemFilter
    , dropScreen
    , win
    , offsetClimb
    , ui
    , neighborsCoords
    , getStyle
    , splitBy
    , gameChanged
    ) where

import Types

import Control.Lens
import Control.Monad (when)
import Control.Monad.State.Strict (State, get, MonadState)
import Data.Array as A
import Data.Map.Strict as M (Map, (!), insert, fromAscList, lookup)
import Data.Maybe (isJust)
import System.Random (StdGen)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import UI.HSCurses.Curses (Window)
import UI.HSCurses.CursesHelper as CH

gameWidth, gameHeight, gameDepth :: Int
gameWidth = 90
gameHeight = 30
gameDepth = 5

debugOn = False

-- identical function signature:
-- player :: Functor f => (Creature -> f Creature) -> Game -> f Game
player :: Lens' Game Creature
player = creatureWithId 0

creature :: Functor f => Creature -> (Creature -> f Creature) -> Game -> f Game
creature c =
    let id = c^.cId
    in creatureWithId id

creatureWithId :: Functor f => Int -> (Creature -> f Creature) -> Game -> f Game
creatureWithId id f game =
    let updateCreature game c' = creatures %~ M.insert id c' $ game
    in fmap (updateCreature game) (f (_creatures game M.! id))

nextInt :: GameState Int
nextInt = curId <+= 1

getStyle :: StyleType -> Game -> CursesStyle
getStyle styleType game = (game^.styles) M.! styleType

ui :: GameState Screen
ui = use $ uis.to last

splitBy :: Int -> [a] -> [[a]]
splitBy width [] = []
splitBy width xs = take width xs : splitBy width (drop width xs)


offsetDir :: Direction -> Coord
offsetDir N = (0, -1, 0)
offsetDir E = (1,  0, 0)
offsetDir S = (0,  1, 0)
offsetDir W = (-1, 0, 0)
offsetDir NE = offsetDir N <+> offsetDir E
offsetDir SE = offsetDir S <+> offsetDir E
offsetDir SW = offsetDir S <+> offsetDir W
offsetDir NW = offsetDir N <+> offsetDir W

offsetClimb :: Climb -> Coord
offsetClimb Up   = (0, 0, -1)
offsetClimb Down = (0, 0,  1)

(<+>) :: Coord -> Coord -> Coord
(x,y,z) <+> (x',y',z') = (x + x',y + y',z+z')

inBounds :: Coord -> Bool
inBounds (x,y,z) = x >= 0
              && y >= 0
              && z >= 0
              && x < gameWidth
              && y < gameHeight
              && z < gameDepth

reverseCoord :: Coord -> Coord
reverseCoord (x, y, z) = (z, y, x)

neighborsCoords :: Coord -> [Coord]
neighborsCoords origin = ixs'
    where dirs = [N,E,S,W,NE,SE,SW,NW]
          offsets = map offsetDir dirs
          ixs = map (origin <+>) offsets
          ixs' = filter inBounds ixs

-- Get a list of all the neighbors of a coordinate
neighbors8 :: Coord -> GameWorld -> [Tile]
neighbors8 origin world = tiles
    where ixs = neighborsCoords origin
          tileAt = (world A.!) . reverseCoord
          tiles = map tileAt ixs

-- | Distance squared
distanceSq :: Coord -> Coord -> Int
distanceSq (x,y,_) (x',y',_) = sq (x - x') + sq (y - y')
    where sq x = x * x

-- | Are two coordinates at the same depth?
sameDepth :: Coord -> Coord -> Bool
sameDepth (_,_,z) (_,_,z') = z == z'

-- | Add a notifiction message, if the coordinate is in range
notify :: Coord -> String -> GameState ()
notify loc s = do
    p <- use player
    let ploc = p^.location
        vision = p^.visionRadius
        inRange = sameDepth loc ploc && distanceSq loc ploc <= vision * vision
    when inRange $ messages %= (s:)

-- | add screen to the game
pushScreen :: Screen -> GameState ()
pushScreen ui = uis %= (++ [ui])

-- | remove the last screen, if there is any
dropScreen :: GameState ()
dropScreen = uis %= safeInit
    where safeInit [] = []
          safeInit xs = init xs

-- | lose the game with message
lose :: String -> GameState ()
lose msg = do
    updated .= False
    loseMessage .= msg
    messages %= (msg:)
    pushScreen Lose

-- | exit the game
quit :: GameState ()
quit = do
    uis .= []
    updated .= False

-- | win the game
win :: GameState ()
win = pushScreen Win

debug :: String -> a -> a
debug str x = if debugOn then debug' str x else x
    where debug' str x = unsafePerformIO $ do
              hPutStrLn stderr str
              return x

-- | Should the game be updated?
gameChanged :: GameState Bool
gameChanged = use updated

effectDone :: Effect -> Bool
effectDone e = e^.effectTime >= e^.effectDuration

dropItemFilter, equipItemFilter, eatItemFilter, examineItemFilter, throwItemFilter, quaffItemFilter, readItemFilter :: Item -> Bool
dropItemFilter = const True
equipItemFilter i = i^.iAttackPower > 0 || i^.iDefensePower > 0
eatItemFilter i = i^.iFoodValue /= 0
examineItemFilter = const True
throwItemFilter i = i^.iThrowAttackPower > 0 || quaffItemFilter i
quaffItemFilter i = isJust $ i^.quaffEffect
readItemFilter i = not $ null $ i^.itemSpells
