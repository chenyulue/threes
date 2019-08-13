module Play where

import System.Random
import Control.Monad.State
import Data.Array
import Data.Maybe (fromJust)
import Graphics.Gloss.Interface.IO.Game
--import Graphics.Gloss.Interface.IO.Interact
import Core
import UI

playGame :: StdGen -> IO ()
playGame g =
  playIO window
         white
         20
         (initialGame g)
         drawGame
         controlGame
         stepGame

drawGame :: Game -> IO Picture
drawGame game =
  if isOver game
  then return $ pictures [ renderGame game
                         , translate (-300) 0 . scale 0.75 1 $ Text "GAME OVER"]
  else return $ renderGame game

renderGame :: Game -> Picture
renderGame game =  
  let arr = board game
      (n, xs) = evalState (nextState game) (1,0)
      maxBlock = maxNum game
   in pictures [ bgBoard
               , nextBlockFrame
               , regionTranslate $ boardToPictures arr maxBlock
               , nextBlocks (n, xs)
               ]
      
boardToPictures :: Array (Int, Int) Int -> Int -> Picture
boardToPictures arr n =
  let xs = [ ((i,j), v) | ((i, j), v) <- assocs arr, v /= 0 ]
   in pictures $ map (pairToPicture n) xs

pairToPicture :: Int -> ((Int, Int), Int) -> Picture
pairToPicture n (pos, v) =
  let (x, y) = fromJust $ lookup pos posMap
      posMap = zip ([(i,j) | j <- [3,2..0], i <- [0..3]]) offSet
   in translate x y $
        if v == n && v > 3
        then blockFunc v color3' color3 colorMax
        else drawBlock v

controlGame :: Event -> Game -> IO Game
controlGame (EventKey (SpecialKey KeyUp) Down _ _) game =
  if isOver game
  then return game
  else up game <$> newStdGen
controlGame (EventKey (SpecialKey KeyDown) Down _ _) game =
  if isOver game
  then return game
  else down game <$> newStdGen
controlGame (EventKey (SpecialKey KeyLeft) Down _ _) game =
  if isOver game
  then return game
  else left game <$> newStdGen
controlGame (EventKey (SpecialKey KeyRight) Down _ _) game =
  if isOver game
  then return game
  else right game <$> newStdGen
controlGame _ game = return game

stepGame :: Float -> Game -> IO Game
stepGame _ = return . id
