{-# LANGUAGE FlexibleContexts #-}
module Core where

import System.Random
import Lens.Micro
import Data.Array
import Data.List (intercalate, groupBy, delete, maximum)
import Control.Monad.State

-- |Directions for moving
data Direction = North | East | South | West

-- |A game type
data Game = Game
  { board :: Board                                   -- ^Game board with values
  , isOver :: Bool                                   -- ^Whether the game is over
  , nextState :: State (Int, Int) (Int, [Int])       -- ^Block in the next step
  , maxNum :: Int                                    -- ^The current maximum
  }

-- |A board is a list of lists
type Board = Array (Int, Int) Int


----------------------------------------------
--        Initiate the Game                 --
----------------------------------------------

-- |@zeroBoard@ is the initial (4*4) board with all values being 0
zeroBoard :: Board
zeroBoard = listArray ((0,0), (3,3)) $ replicate 16 0

-- |Initiate the game board with 9 random values between 1 and 3,
-- |with at leat two 3s, and at least one 1 and one 2
initialBoard :: StdGen -> Board
initialBoard g = 
  let (num3, g') = randomR (2,4) g     -- Number of 3
      (num2, g'') = randomR (1,4) g'   -- Number of 2
      num1 = 9 - num3 - num2           -- Number of 1
      pos = distinctTake 9 $ randomRs ((0,0), (3,3)) g''   -- 9 initial positions for the initial values
      values = zip pos (replicate num3 3 ++ replicate num2 2 ++ replicate num1 1)
   in zeroBoard // values

-- |@distinctTake n xs@ takes the first @n@ elements from the list @xs@.      
distinctTake :: Eq a => Int -> [a] -> [a]
distinctTake n ys = helper n [] ys
  where
    helper 0 rev _ = rev
    helper _ rev [] = rev
    helper n rev (x:xs) | x `elem` rev = helper n rev xs
                        | otherwise = helper (n-1) (x:rev) xs

-- |Convert the board to a string for printing.
showBoard :: Board -> String
showBoard arr =
  let blocks = groupBy (\x y -> (x ^. _1 . _1) == (y ^. _1 . _1)) . assocs $ arr
   in unlines . map (intercalate " " . map (show . snd)) $ blocks
      
initialGame :: StdGen -> Game
initialGame g =
  let initialB = initialBoard g
   in Game initialB False (nextValue 3 g) 3

----------------------------------------------------
--             Generate the next value            --
----------------------------------------------------

-- |According to the current maximum, generate the next value
nextValue :: Int -> StdGen -> State (Int, Int) (Int, [Int])
nextValue m g = do
  (previousNum, times) <- get
  if m < 192
    then generateNext previousNum times [1..3] g
    else let (n, g') = randomR (1, 20 :: Int) g
          in case n of
               5 -> generateNext previousNum times [6,12,24] g'
               4 -> generateNext previousNum times [6,12] g'
               _ -> generateNext previousNum times [1..3] g'

-- |@generateNext pre n xs g@ generates a random value according to @pre@ and @n@
-- |from a list @xs@, with each random number generated continually less than 3 times.
generateNext :: Int -> Int -> [Int] -> StdGen -> State (Int, Int) (Int, [Int])
generateNext previousNum times xs g = do
  let (n, g') = choose xs g
  if times >= 1                                          -- If n has been generated 3 times or more, then choose a different n.
    then let (m, g'') = choose (delete previousNum xs) g'
          in modify (const (m,1)) >> return (m, xs)
    else if n == previousNum                             -- Else, use the generated random number, and update the state.
           then modify (& _2 %~ (+1)) >> return (n, xs)
           else modify (const (n,1)) >> return (n, xs)
        
-- |@choose xs g@ chooses a random value from a list @xs@.
choose :: [a] -> StdGen -> (a, StdGen)
choose [] _ = error "empty list"
choose xs g = let (n, g') = randomR (0, length xs - 1) g
               in (xs !! n, g')

----------------------------------------------------------
--                    Move the Game                     --
----------------------------------------------------------

canMerge :: ((Int, Int), Int) -> ((Int, Int), Int) -> Bool
canMerge block1 block2 =
  let x = snd block1
      y = snd block2
   in x == 0 || (x == 1 && y == 2) ||
      (x == 2 && y == 1) || (x >= 3 && y >= 3 && x == y)

move :: Direction -> [((Int, Int), Int)] -> [((Int, Int), Int)]
move _ [] = []
move North xs =
  let (y, ys) = (head xs, tail xs)
   in if (not . null $ ys) && (canMerge y (head ys))
         then (fst y, snd y + snd (head ys)) : map (& _1 . _1 %~ (subtract 1)) (tail ys)
         else y : move North ys
move South xs =
  let (y, ys) = (last xs, init xs)
   in if (not . null $ ys) && (canMerge y (last ys))
         then map (& _1 . _1 %~ (+1)) (init ys) ++ [(fst y, snd y + snd (last ys))]
         else move South ys ++ [y]
move West xs =
  let (y, ys) = (head xs, tail xs)
   in if (not . null $ ys) && (canMerge y (head ys))
         then (fst y, snd y + snd (head ys)) : map (& _1 . _2 %~ (subtract 1)) (tail ys)
         else y : move West ys
move East xs =
  let (y, ys) = (last xs, init xs)
   in if (not . null $ ys) && (canMerge y (last ys))
         then map (& _1 . _2 %~ (+1)) (init ys) ++ [(fst y, snd y + snd (last ys))]
         else move East ys ++ [y]

up, down, left, right :: Game -> StdGen -> Game
up game g =
  let arr = board game
      arrAfterMove = (zeroBoard //) . concat . map (move North) $ map (column arr) [0..3]
      lastZeros = [((i,j),v) | ((i,j),v) <- assocs arrAfterMove, i == 3 && v == 0]
      newBoard = arrAfterMove // (newBlock lastZeros (nextState game) g)
      finished = not $ check newBoard
      maxValue = getMax newBoard
      newState = nextValue maxValue g
   in Game newBoard finished newState maxValue
down game g =
  let arr = board game
      arrAfterMove = (zeroBoard //) . concat . map (move South) $ map (column arr) [0..3]
      lastZeros = [((i,j),v) | ((i,j), v) <- assocs arrAfterMove, i == 0 && v == 0]
      newBoard = arrAfterMove // (newBlock lastZeros (nextState game) g)
      finished = not $ check newBoard
      maxValue = getMax newBoard
      newState = nextValue maxValue g
   in Game newBoard finished newState maxValue
left game g =
  let arr = board game
      arrAfterMove = (zeroBoard //) . concat . map (move West) $ map (row arr) [0..3]
      lastZeros = [((i,j),v) | ((i,j), v) <- assocs arrAfterMove, j == 3 && v == 0]
      newBoard = arrAfterMove // (newBlock lastZeros (nextState game) g)
      finished = not $ check newBoard
      maxValue = getMax newBoard
      newState = nextValue maxValue g
   in Game newBoard finished newState maxValue
right game g =
  let arr = board game
      arrAfterMove = (zeroBoard //) . concat . map (move East) $ map (row arr) [0..3]
      lastZeros = [((i,j),v) | ((i,j),v) <- assocs arrAfterMove, j == 0 && v == 0]
      newBoard = arrAfterMove // (newBlock lastZeros (nextState game) g)
      finished = not $ check newBoard
      maxValue = getMax newBoard
      newState = nextValue maxValue g
   in Game newBoard finished newState maxValue

newBlock :: [((Int, Int), Int)] -> State (Int, Int) (Int, [Int]) -> StdGen -> [((Int, Int), Int)]
newBlock arr s g =
  let n = length arr
      (pos, g') = randomR (0, n-1) g
      (v, _) = evalState s (1,0)
   in arr & ix pos . _2 .~ v
   
getMax :: Board -> Int
getMax = maximum . elems

check :: Board -> Bool
check arr = or . map canMove . assocs $ arr
  where
    canMove v = or . map (moveQ v) $ getFourSide (fst v)
    getFourSide (i,j) = [(i-1, j), (i+1, j), (i, j-1), (i, j+1)]
    moveQ v (i,j)
      | i < 0 || j < 0 || i > 3 || j > 3 = False
      | otherwise  = canMerge ((i,j),(arr ! (i,j))) v

column, row :: Board -> Int -> [((Int, Int), Int)]
column arr n = [((i,j), v) | ((i,j), v) <- assocs arr, j == n]
row arr n = [((i,j), v) | ((i,j), v) <- assocs arr, i == n]

instance (Random a, Random b) => Random (a, b) where
  random g =
    let (x, g') = random g
        (y, g'') = random g'
     in ((x,y), g'')
  randomR ((x0,y0), (x1, y1)) g =
    let (x, g') = randomR (x0, x1) g
        (y, g'') = randomR (y0, y1) g'
     in ((x,y), g'')
