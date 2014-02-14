module Draw where

import Control.Lens
import Control.Monad (forM_, when)
import Control.Monad.State.Strict as S
import Data.Array as A
import Data.Map.Strict as M (elems, filter)
import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper

import Game

type GameIOState = StateT Game IO

drawGame :: GameIOState ()
drawGame = do
    game <- get
    resetColor
    drawScreen (ui game)
    liftIO refresh

drawBlock :: Int -> Int -> [String] -> GameIOState ()
drawBlock y x strs = get >>= \game ->
    mapM_  (\(i, s) -> liftIO $ mvWAddStr (game^.window) (y + i) x s)
    (zip [0..] strs)

drawStr :: Int -> Int -> String -> GameIOState ()
drawStr y x str = get >>= \game -> liftIO $ mvWAddStr (game^.window) y x str

drawScreen :: Screen -> GameIOState ()
drawScreen Start = do
    drawStr 4 4 "Welcome to the Caves of Slight Danger"
    drawBlock 15 4 [ "Press [enter] to Start Playing"
                   , "Press [q] to Quit" ]

drawScreen Win = do
    drawStr 4 4 "You WIN!"
    drawBlock 15 4 [ "Press [esc] to Quit"
                   , "Press <Anything> to Go back to Start" ]

drawScreen Lose = do
    drawStr 4 4 "You LOSE!"
    drawBlock 15 4 [ "Press [esc] to Quit"
                   , "Press <Anything> to Go back to Start" ]

drawScreen Play = do
    drawLevel
    drawPlayer
    drawCreatures
    drawHud
    drawMessages

drawMessages :: GameIOState ()
drawMessages = get >>= \game -> do
    forM_ (zip [1..] (game^.messages)) $ \(i, msg) -> drawStr (gameHeight + i) 5 msg
    messages .= []

drawHud :: GameIOState ()
drawHud = get >>= \game -> do
    let p = game^.player
    resetColor
    drawStr gameHeight 0 $
        "loc=" ++ show (p^.location) ++ " hp=[" ++ show (p^.hp) ++ "/" ++ show (p^.maxHp) ++ "]"

getOffsets :: GameIOState (Int, Int)
getOffsets = do
    game <- get
    (sh,sw) <- liftIO scrSize
    let (px,py,_) = game^.player^.location
        offsetX = max 0 (min (px - (sw `div` 2)) (gameWidth - sw))
        offsetY = max 0 (min (py - (sh `div` 2)) (gameHeight - sh))
    return (offsetX, offsetY)

drawCreatures :: GameIOState ()
drawCreatures = do
    game <- get
    forM_ (M.elems $ M.filter (sameDepth (game^.player)) $ game^.creatures) drawCreature
    where sameDepth p c = depth == playerDepth
            where (_,_,depth) = c^.location
                  (_,_,playerDepth) = p^.location

block :: Int -> Int -> [a] -> [a]
block offset size = take size . drop offset

block2d :: (Int,Int) -> (Int,Int) -> [[a]] -> [[a]]
block2d (xOffset,width) (yOffset,height) xs = map (Draw.block xOffset width) (Draw.block yOffset height xs)

drawLevel :: GameIOState ()
drawLevel = do
    game <- get
    offsets <- getOffsets
    sSize <- liftIO scrSize
    let (_,_,depth) = game^.player.location
        lvl2d = (splitBy (gameWidth * gameHeight) $ A.elems $ game^.world) !! depth
        rows = splitBy gameWidth lvl2d
        blocks (ox,oy) (sh,sw) = block2d (ox, sw) (oy, sh) rows
        lvl offsets sSize = map row2str (blocks offsets sSize)
        row2str = foldr (\tile str -> (tile^.glyph) : str) ""
    drawBlock 0 0 (lvl offsets sSize)

resetColor :: GameIOState ()
resetColor = liftIO resetStyle

inScreenBounds :: Int -> Int -> GameIOState Bool
inScreenBounds gx gy = do
    (sx,sy) <- getScreenCoords gx gy
    (sh,sw) <- liftIO scrSize
    return $ sx >= 0 && sx < sw
          && sy >= 0 && sy < sh

getScreenCoords:: Int -> Int -> GameIOState (Int, Int)
getScreenCoords x y = do
    (ox,oy) <- getOffsets
    return (x - ox, y - oy)

drawCreature :: Creature -> GameIOState ()
drawCreature creature = do
    game <- get
    let (x,y,_) = creature^.location
    visible <- inScreenBounds x y
    when visible $ do
        (sx,sy) <- getScreenCoords x y
        let glyph = creature^.c_glyph
            cstyle = nthStyle (creature^.c_style) game
        liftIO $ setStyle cstyle
        drawStr sy sx [glyph]

drawPlayer :: GameIOState ()
drawPlayer = get >>= \game -> drawCreature (game^.player)
