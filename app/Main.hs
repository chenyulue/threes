module Main where

import System.Random
import Play

main :: IO ()
main = do
  g <- newStdGen
  playGame g
