module Random
    ( Random.randomR
    , randomL
    ) where

import Control.Lens
import Control.Monad.State.Strict (get)
import System.Random as R (randomR)

import Game

-- | Random number in a range
randomR :: (Int,Int) -> GameState Int
randomR range = do
    g <- use stdGen
    let (x, g') = R.randomR range g
    stdGen .= g'
    return x

-- | Random element from a list
randomL :: [a] -> GameState a
randomL xs = do
    g  <- use stdGen
    let (ix, g') = R.randomR (0, length xs - 1) g
    stdGen .= g'
    return $ xs !! ix
