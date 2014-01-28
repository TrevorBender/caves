module Main where

import System.Console.ANSI



main :: IO ()
main = do
    setSGR [ SetConsoleIntensity BoldIntensity
           , SetColor Foreground Vivid Blue ]
    putStrLn "@"
