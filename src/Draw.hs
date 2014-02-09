module Draw where

import Control.Lens
import Control.Monad (forM_, when)
import Data.Array as A
import Data.Map.Strict as M (elems, filter)
import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper

import Game

drawGame :: Game -> IO ()
drawGame game = do
    drawScreen (ui game) game
    refresh

drawBlock :: Game -> Int -> Int -> [String] -> IO ()
drawBlock game y x = (mapM_ $ \(i, s) -> do
    mvWAddStr (game^.window) (y + i) x s
    ) . zip [0..]

drawStr :: Game -> Int -> Int -> String -> IO ()
drawStr game y x str = mvWAddStr (game^.window) y x str

drawScreen :: Screen -> Game -> IO ()
drawScreen Start game = do
    drawStr game 4 4 "Welcome to the Caves of Slight Danger"
    drawBlock game 15 4 [ "Press [enter] to Start Playing"
                        , "Press [q] to Quit" ]

drawScreen Win game = do
    drawStr game 4 4 "You WIN!"
    drawBlock game 15 4 [ "Press [esc] to Quit"
                        , "Press <Anything> to Go back to Start" ]

drawScreen Lose game = do
    drawStr game 4 4 "You LOSE!"
    drawBlock game 15 4 [ "Press [esc] to Quit"
                        , "Press <Anything> to Go back to Start" ]

drawScreen Play game = do
    drawLevel game
    drawPlayer game
    drawCreatures game
    drawHud game

drawHud :: Game -> IO ()
drawHud game = do
    let p = game^.player
    resetColor
    drawStr game gameHeight 0 $
        "loc=" ++ show (p^.location) ++ " hp=[" ++ show (p^.hp) ++ "/" ++ show (p^.maxHp) ++ "]"

getOffsets :: Game -> IO (Int, Int)
getOffsets game = do
    (sh,sw) <- scrSize
    let (px,py,_) = game^.player^.location
        offsetX = max 0 (min (px - (sw `div` 2)) (gameWidth - sw))
        offsetY = max 0 (min (py - (sh `div` 2)) (gameHeight - sh))
    return (offsetX, offsetY)

drawCreatures :: Game -> IO ()
drawCreatures game = do
    forM_ (M.elems $ M.filter sameDepth $ game^.creatures) (drawCreature game)
    where sameDepth c = depth == playerDepth
            where (_,_,depth) = c^.location
                  (_,_,playerDepth) = game^.player^.location

block :: Int -> Int -> [a] -> [a]
block offset size = take size . drop offset

block2d :: (Int,Int) -> (Int,Int) -> [[a]] -> [[a]]
block2d (xOffset,width) (yOffset,height) xs = map (Draw.block xOffset width) (Draw.block yOffset height xs)

drawLevel :: Game -> IO ()
drawLevel game = do
    offsets <- getOffsets game
    sSize <- scrSize
    drawBlock game 0 0 (lvl offsets sSize)
    where (_,_,depth) = game^.player.location
          lvl2d = (splitBy (gameWidth * gameHeight) $ A.elems $ game^.world) !! depth
          rows = splitBy gameWidth lvl2d
          blocks (ox,oy) (sh,sw) = block2d (ox, sw) (oy, sh) rows
          lvl offsets sSize = map row2str (blocks offsets sSize)
          row2str = foldr (\tile str -> (tile^.glyph) : str) ""

resetColor :: IO ()
resetColor = resetStyle

inScreenBounds :: Game -> Int -> Int -> IO Bool
inScreenBounds game gx gy = do
    (sx,sy) <- getScreenCoords game gx gy
    (sh,sw) <- scrSize
    return $ sx >= 0 && sx < sw
          && sy >= 0 && sy < sh

getScreenCoords:: Game -> Int -> Int -> IO (Int, Int)
getScreenCoords game x y = do
    (ox,oy) <- getOffsets game
    return (x - ox, y - oy)

drawCreature :: Game -> Creature -> IO ()
drawCreature game creature = do
    let (x,y,_) = creature^.location
    visible <- inScreenBounds game x y
    when visible $ do
        (sx,sy) <- getScreenCoords game x y
        let glyph = creature^.c_glyph
            cstyle = nthStyle (creature^.c_style) game
        setStyle cstyle
        drawStr game sy sx [glyph]

drawPlayer :: Game -> IO ()
drawPlayer game = drawCreature game (game^.player)
