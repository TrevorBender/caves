module Random where

import Control.Lens
import Control.Monad.State.Strict (get)
import System.Random as R (randomR)

import Game

randomR :: (Int,Int) -> GameState Int
randomR range = do
    game <- get
    let g = game^.stdGen
        (x, g') = R.randomR range g
    stdGen .= g'
    return x

randomL :: [a] -> GameState a
randomL xs = do
    game <- get
    let g = game^.stdGen
        (ix, g') = R.randomR (0,(length xs) - 1) g
    stdGen .= g'
    return $ xs !! ix