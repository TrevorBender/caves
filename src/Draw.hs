module Draw
    ( drawGame
    , resetColor
    , toChar
    ) where

import Prelude as P
import Control.Lens
import Control.Monad (forM_, when)
import Control.Monad.State.Strict as S
import Data.Array as A
import Data.Char (chr, ord, intToDigit)
import Data.Map.Strict as M (elems, filter, assocs, filterWithKey)
import Data.Maybe (isJust)
import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper

import Game
import World (tileAt, unknownTile, canSee)
import Creature (creatureAttack, creatureDefense, levelUpStrings)

type GameIOState = StateT Game IO

-- TODO use uis + forM_ uis , better way?
drawGame :: GameIOState ()
drawGame = do
    uis <- use uis
    resetColor
    forM_ uis drawScreen
    liftIO refresh

drawBlock :: Int -> Int -> [String] -> GameIOState ()
drawBlock y x strs = do
    w <- use window
    mapM_  (\(i, s) -> liftIO $ mvWAddStr w (y + i) x s) (zip [0..] strs)

drawStr :: Int -> Int -> String -> GameIOState ()
drawStr y x str = do
    w <- use window
    liftIO $ mvWAddStr w y x str

drawScreen :: Screen -> GameIOState ()
drawScreen Start = do
    drawBlock 4 4 [ "You stumble and fall into the Caverns of Extreme Danger"
                  , "The only way out is to collect the idol from the bottom level"
                  , "before climbing the stairs back to the surface"
                  ]
    drawBlock 15 4 [ "Press [any key] to Start Playing"
                   ]

drawScreen Win = drawStr 4 4 "You WIN!"

drawScreen Lose = drawStr 4 4 "You LOSE!"

drawScreen Play = do
    drawLevel
    drawItems
    drawPlayer
    drawCreatures
    drawHud
    drawMessages

drawScreen DropItem = drawInventoryScreen "Drop Item" dropItemFilter

drawScreen EquipItem = drawInventoryScreen "Equip Item" equipItemFilter

drawScreen EatItem = drawInventoryScreen "Eat Item" eatItemFilter

drawScreen ChooseLevelUp = do
    drawStr 5 5 "Choose Level Up"
    drawStr 6 5 "               "
    drawBlock 7 5 levelUpStrings

drawScreen Help =
    drawBlock 5 5 [ "Help"
                  , "The only way out is to collect the idol from the bottom level"
                  , "before climbing the stairs back to the surface"
                  , ""
                  , "[,] pick up item"
                  , "[d] drop item"
                  , "[e] eat item"
                  , "[x] equip item"
                  , "[?] for this help"
                  , ""
                  , "-- press any key to continue --"
                  ]

drawInventoryScreen :: String -> (Item -> Bool) -> GameIOState ()
drawInventoryScreen str filt = do
    drawStr 5 5  str
    drawStr 6 5 "                "
    p <- use player
    drawBlock 7 5 $ itemStrings filt [p^.weapon, p^.armor] $ p^.inventory


itemStrings :: (Item -> Bool) -> [Maybe Item] -> [Item] -> [String]
itemStrings filt mes = map i2s . P.filter (\(_,i) -> filt i) . zip ['a'..]
    where i2s (c, i) = c : (" - " ++ (i^.i_name) ++ if elem (Just i) mes then " *" else "" )

drawItem :: Item -> GameIOState ()
drawItem item = do
    game <- get
    let (x,y,_) = item^.i_location
        glyph = item^.i_glyph
        cstyle = getStyle (item^.i_style) game
    visible <- inScreenBounds x y
    when visible $ do
        (sx,sy) <- getScreenCoords x y
        liftIO $ setStyle cstyle
        drawStr sy sx [glyph]

drawItems :: GameIOState ()
drawItems = do
    game <- get
    p <- use player
    is <- use items
    let gz = p^.location
        visibleItem loc item = canSee game loc p
        visItems = M.filterWithKey visibleItem is
    forM_ (M.elems visItems) drawItem

drawMessages :: GameIOState ()
drawMessages = do
    ms <- use messages
    forM_ (zip [1..] (reverse $ ms)) $ \(i, msg) -> drawStr (gameHeight + i) 5 msg
    messages .= []

drawHud :: GameIOState ()
drawHud = do
    p <- use player
    let loc = p^.location
        (_,_,depth) = loc
        health = show (p^.hp) ++ "/" ++ show (p^.maxHp)
        inv = show (length (p^.inventory)) ++ "/" ++ show (p^.maxInv)
        ap = show $ creatureAttack p
        def = show $ creatureDefense p
        levelStr = show $ p^.level
    resetColor
    drawStr gameHeight 0 $
        "depth=" ++ show (depth + 1) ++ " hp=[" ++ health ++ "] inv=[" ++ inv ++ "] ap=" ++ ap ++ " def=" ++ def ++ " level=" ++ levelStr ++ " " ++ hunger p

    where hunger p =
              let f  = fromIntegral $ p^.food
                  mf = fromIntegral $ p^.maxFood
              in if      f < mf * 0.1 then "Starving"
                 else if f < mf * 0.2 then "Hungry"
                 else if f > mf * 0.9 then "Stuffed"
                 else if f > mf * 0.8 then "Full"
                 else ""

getOffsets :: GameIOState (Int, Int)
getOffsets = do
    loc <- use $ player.location
    (sh,sw) <- liftIO scrSize
    let (px,py,_) = loc
        offsetX = max 0 (min (px - (sw `div` 2)) (gameWidth - sw))
        offsetY = max 0 (min (py - (sh `div` 2)) (gameHeight - sh))
    return (offsetX, offsetY)

drawCreatures :: GameIOState ()
drawCreatures = do
    p <- use player
    cs <- use creatures
    forM_ (M.elems $ M.filter (sameDepth p) $ cs) drawCreature
    where sameDepth p c = depth == playerDepth
            where (_,_,depth) = c^.location
                  (_,_,playerDepth) = p^.location

block :: Int -> Int -> [a] -> [a]
block offset size = take size . drop offset

block2d :: (Int,Int) -> (Int,Int) -> [[a]] -> [[a]]
block2d (xOffset,width) (yOffset,height) xs = map (Draw.block xOffset width) (Draw.block yOffset height xs)

toChar :: Int -> String
toChar i = [ch]
    where ch = if i < 10 then intToDigit i else
               if i < 10 + 25 then chr $ i - 10 + ord 'A' else
               if i < 10 + 25 + 25 then chr $ i -10 - 25 + ord 'a' else '*'

drawRegionNumbers :: GameIOState ()
drawRegionNumbers = do
    loc <- use $ player.location
    rm <- use regionMap
    (ox,oy) <- getOffsets
    (sh,sw) <- liftIO scrSize
    let (_,_,depth) = loc
        (_,rMap,_) = rm
        regionAssocs = P.filter (\((x,y,z),mr) -> z == depth && isJust mr) (M.assocs rMap)
        regionAssocs' = map (\((x,y,z),Just r) -> ((x-ox, y-oy), r)) regionAssocs
    forM_ regionAssocs' $ \((x,y), num) -> do
        inBounds <- inScreenBounds x y
        if inBounds then drawStr y x (toChar num) else return ()

drawLevel :: GameIOState ()
drawLevel = do
    game <- get
    offsets@(ox,oy) <- getOffsets
    sSize <- liftIO scrSize
    let cstyle = getStyle OutOfSiteStyle game
    liftIO $ setStyle cstyle
    let (_,_,depth) = game^.player.location
        lvl2d = (splitBy (gameWidth * gameHeight) $ A.elems $ game^.visibleWorld) !! depth
        rows = splitBy gameWidth lvl2d
        blocks (ox,oy) (sh,sw) = block2d (ox, sw) (oy, sh) rows
        lvl offsets sSize = map row2str (blocks offsets sSize)
        row2str = foldr (\tile str -> (tile^.glyph) : str) ""
    drawBlock 0 0 (lvl offsets sSize)
    let visibleTiles = P.filter (\(loc,tile) -> canSee game (reverseCoord loc) (game^.player)) (A.assocs $ game^.world)
        visibleTiles' = map (\((_,y,x), tile) -> ((x-ox, y-oy), tile)) visibleTiles
    resetColor
    forM_ visibleTiles' $ \((x,y), tile) ->
        drawStr y x [tile^.glyph]

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
    let loc@(x,y,_) = creature^.location
    visible <- inScreenBounds x y
    when (visible && canSee game loc (game^.player)) $ do
        (sx,sy) <- getScreenCoords x y
        let glyph = creature^.c_glyph
            cstyle = getStyle (creature^.c_style) game
        liftIO $ setStyle cstyle
        drawStr sy sx [glyph]

drawPlayer :: GameIOState ()
drawPlayer = use player >>= drawCreature
