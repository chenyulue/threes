module Main where

import Data.IORef
import Data.Array
import System.Random
import Control.Monad.State

import Core

main :: IO ()
main = do
  game <- initialGame <$> newStdGen
  putStrLn "The initial Game:"
  putStrLn . showBoard . board $ game
  mainMove game

mainMove :: Game -> IO ()
mainMove game = do
  gameR <- newIORef game
  g' <- readIORef gameR
  if isOver g'
    then putStrLn "Game Over!\n Bye!"
    else getLine >>=  flip moveGame gameR

moveGame :: String -> IORef Game -> IO ()
moveGame "j" game = do
  g <- newStdGen
  modifyIORef game (flip left g)
  g' <- readIORef game
  putStrLn $ "After move left, maximum is " ++ show (maxNum g')
  let v = evalState (nextState g') (1,0)
  putStrLn $ "Next block is " ++ show v
  putStrLn . showBoard . board $ g'
  mainMove g'
moveGame "l" game = do
  g <- newStdGen
  modifyIORef game (flip right g)
  g' <- readIORef game
  putStrLn $ "After move right, maximum is " ++ show (maxNum g')
  let v = evalState (nextState g') (1,0)
  putStrLn $ "Next block is " ++ show v
  putStrLn . showBoard . board $ g'
  mainMove g'
moveGame "i" game = do
  g <- newStdGen
  modifyIORef game (flip up g)
  g' <- readIORef game
  putStrLn $ "After move up, maximum is " ++ show (maxNum g')
  let v = evalState (nextState g') (1,0)
  putStrLn $ "Next block is " ++ show v
  putStrLn . showBoard . board $ g'
  mainMove g'
moveGame "k" game = do
  g <- newStdGen
  modifyIORef game (flip down g)
  g' <- readIORef game
  putStrLn $ "After move down, maximum is " ++ show (maxNum g')
  let v = evalState (nextState g') (1,0)
  putStrLn $ "Next block is " ++ show v
  putStrLn . showBoard . board $ g'
  mainMove g'
