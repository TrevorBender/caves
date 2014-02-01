{-# LANGUAGE TemplateHaskell #-}

module Main where

import Prelude hiding (floor)
import System.Console.ANSI
import Control.Lens
import System.IO (hGetChar, hGetEcho, hSetEcho, stdin)
import System.Random (getStdGen, randomR, randomRs, RandomGen(..))

-- GAME -----------------------------------------------------------------------{{{

data Screen = Start | Win | Lose | Play deriving (Show)

data TileKind = Floor | Wall deriving (Show)

data Tile = Tile
    { _kind  :: TileKind
    , _glyph :: Char
    } deriving (Show)

type Row = [Tile]
type GameLevel = [Row]

data Game = Game
    { _uis   :: [Screen]
    , _level :: GameLevel
    } deriving (Show)

makeLenses ''Game
makeLenses ''Tile

ui :: Game -> Screen
ui game = head $ game^.uis

emptyUis :: Game -> Bool
emptyUis game = null $ game^.uis

createGame :: IO Game
createGame = do
    level <- createLevel
    return $ Game { _uis = [ Start ]
                  , _level = level
                  }


floor = Tile { _kind = Floor , _glyph = '.' }
wall = Tile { _kind = Wall , _glyph = '#' }

int2Tile :: Int -> Tile
int2Tile n = [floor, wall] !! n

splitBy :: Int -> [a] -> [[a]]
splitBy width [] = []
splitBy width xs = (take width xs) : (splitBy width (drop width xs))

randomLevel :: RandomGen g => Int -> Int -> g -> GameLevel
randomLevel width height g = splitBy width tiles
    where rs = take (width * height) (randomRs (0, 1) g)
          tiles = map int2Tile rs

gameWidth = 80
gameHeight = 30

createLevel :: IO GameLevel
createLevel = do
    g <- getStdGen
    return $ randomLevel gameWidth gameHeight g

--}}}

-- DRAWING --------------------------------------------------------------------{{{
drawScreen :: Screen -> Game -> IO ()
drawScreen Start _ = do
    setCursorPosition 4 4
    putStr "Welcome to the Caves of Slight Danger"
    setCursorPosition 15 4
    putStr "Press [enter] to Start Playing"
    setCursorPosition 16 4
    putStr "Press [q] to Quit"

drawScreen Win _ = do
    setCursorPosition 4 4
    putStr "You WIN!"
    setCursorPosition 15 4
    putStr "Press [esc] to Quit"
    setCursorPosition 16 4
    putStr "Press <Anything> to Go back to Start"

drawScreen Lose _ = do
    setCursorPosition 4 4
    putStr "You LOSE!"
    setCursorPosition 15 4
    putStr "Press [esc] to Quit"
    setCursorPosition 16 4
    putStr "Press <Anything> to Go back to Start"

drawScreen Play game = do
    setCursorPosition 0 0
    drawLevel game

drawLevel :: Game -> IO ()
drawLevel game = do
    mapM_ (\r -> putStrLn (map (\t -> t^.glyph) r)) (game^.level)

drawGame :: Game -> IO ()
drawGame game = drawScreen (ui game) game
--}}}

-- INPUT ----------------------------------------------------------------------{{{
getInput :: IO Char
getInput = getChar

processInputScreen :: Screen -> Char -> Game -> Game
processInputScreen Start ch game =
    case ch of
         '\n' -> (uis .~ [Play]) game
         'q' -> (uis .~ []) game
         _ -> game

processInputScreen Win ch game =
    case ch of
         '\ESC' -> (uis .~ []) game
         _ -> (uis .~ [Start]) game

processInputScreen Lose ch game =
    case ch of
         '\ESC' -> (uis .~ []) game
         _ -> (uis .~ [Start]) game

processInputScreen Play ch game =
    case ch of
         '\n' -> (uis .~ [Win]) game
         '\DEL' -> (uis .~ [Lose]) game
         'q' -> (uis .~ []) game
         _ -> game

processInput :: Char -> Game -> Game
processInput ch game = processInputScreen (ui game) ch game
--}}}

gameLoop :: Game -> IO ()
gameLoop game = do
    clearScreen
    drawGame game
    ch <- getInput
    let game' = processInput ch game
    if (emptyUis game')
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

drawHero :: IO ()
drawHero = do
    setSGR [ SetConsoleIntensity BoldIntensity
           , SetColor Foreground Vivid Blue ]
    putStrLn "@"
