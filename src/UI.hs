module UI where

import Graphics.Gloss

-- |the background color for game region 
bgColor :: Color
bgColor = makeColorI 204 230 227 255

-- |the backgraound color for block frames
bgFrameColor :: Color
bgFrameColor = makeColorI 184 219 215 255

-- |Set the window size and position
windowSize :: (Float, Float)
windowSize = (600, snd gameRegionSize + 2 * gameRegionOffset)

windowPos :: (Int, Int)
windowPos = (100, 10)

window :: Display
window = InWindow "Threes" (both round windowSize) windowPos
  where both f (x, y) = (f x, f y)

-------------------------------------------------------
--          The empty game board                     --
-------------------------------------------------------
gameRegionX, goldRatio :: Float
gameRegionX = 370
goldRatio = 0.618

-- |Set the game region
gameRegionSize :: (Float, Float)
gameRegionSize = (gameRegionX, gameRegionX / goldRatio)

gameRegionOffset :: Float
gameRegionOffset = 10

regionTranslate :: Picture -> Picture
regionTranslate = translate ( fst windowSize / 2
                              - gameRegionOffset
                              - fst gameRegionSize / 2)
                            0

gameRegion :: Picture
gameRegion = regionTranslate $ color bgColor (uncurry rectangleSolid gameRegionSize)

-- |Set the block frames
frameSize :: (Float, Float)
frameSize = (80, 80 / goldRatio)

frameSpace :: (Float, Float)
frameSpace = (10, 10 / goldRatio)

frame :: Picture
frame = regionTranslate $ color bgFrameColor (uncurry rectangleSolid frameSize)

-- |The offsets for the 16 frames
offSet :: [(Float, Float)]
offSet =
  let topX = fst gameRegionSize / 2 - fst frameSpace - fst frameSize / 2
      topY = snd gameRegionSize / 2 - snd frameSpace - snd frameSize / 2
      topXs = map (\x -> (topX - x * fst frameSpace - x * fst frameSize)) [0..3]
      topYs = map (\x -> (topY - x * snd frameSpace - x * snd frameSize)) [0..3]
   in [(x,y) | x <- topXs, y <- topYs]
      
-- |The game board backgrand
bgBoard :: Picture
bgBoard = Pictures $ gameRegion : map (\(x,y) -> translate x y frame) offSet


-------------------------------------------------------------
--                   The Active blocks                     --
-------------------------------------------------------------

-- |Color for active blocks
color1, color1', color2, color2', color3, color3' :: Color
-- For number 1
color1 = makeColorI 113 202 242 255
color1' = makeColorI 106 166 218 255
-- For number 2
color2 = makeColorI 241 102 128 255
color2' = makeColorI 205 83 124 255
-- For number 3 or more
color3 = white
color3' = makeColorI 253 216 102 255

-- |Color for numbers
colorOne, colorTwo, colorThree, colorMax :: Color
colorOne = white
colorTwo = white
colorThree = black
colorMax = makeColorI 255 98 128 255

-- |Set active blocks
drawBlock :: Int -> Picture
drawBlock n
  | n == 1 = blockFunc 1 color1' color1 colorOne
  | n == 2 = blockFunc 2 color2' color2 colorTwo
  | otherwise = blockFunc n color3' color3 colorThree

blockFunc :: Int -> Color -> Color -> Color -> Picture
blockFunc n bColor tColor sColor = Pictures [ color bColor bottomBlock
                                            , color tColor topBlock
                                            , color sColor $ translateNum n
                                                           $ scale 0.15 0.5 (Text . show $ n)]
  where
    radius = 6
    space = 7
    centerX = fst frameSize - 2*radius
    centerY = snd frameSize - space
    sideY = centerY - 2 * radius
    topBlock = Pictures [ centerRectangle
                        , translate (centerX / 2 + radius / 2) 0 sideRectangle
                        , translate (-centerX / 2 - radius / 2) 0 sideRectangle
                        , translate (-centerX / 2) (sideY / 2) sector
                        , translate (centerX / 2) (sideY / 2) $ rotate 90 sector
                        , translate (centerX / 2) (-sideY / 2) $ rotate 180 sector
                        , translate (-centerX / 2) (-sideY / 2) $ rotate 270 sector]
    bottomBlock = translate 0 (-space) topBlock
    centerRectangle = rectangleSolid centerX centerY
    sideRectangle = rectangleSolid radius sideY
    sector = arcSolid 90 180 radius
    translateNum n
      | n < 10 = translate (-7) (-25)
      | n < 100 = translate (-11) (-25)
      | n < 1000 = translate (-15) (-25)
      | n < 10000 = translate (-21) (-25)
      | n < 100000 = translate (-28) (-25)
      | n < 1000000 = translate (-33) (-25)
      | otherwise = translate (-39) (-25)

--------------------------------------------------------
--           The Next Block                           --
--------------------------------------------------------
sectorRadius, nextBlockSpace :: Float
sectorRadius = fst frameSize / 2
nextBlockSpace = 30
rectX = fst windowSize - gameRegionOffset - fst gameRegionSize
         - nextBlockSpace - sectorRadius
rectY = fst frameSize

nextBlockFrame :: Picture
nextBlockFrame = pictures [ color bgColor $ translate (-x) 0 rect
                          , color bgColor $ translate (-x') 0 semiCircle]
  where
    rect = rectangleSolid rectX rectY
    semiCircle = rotate 180 $ arcSolid 90 (-90) sectorRadius
    x = fst windowSize / 2 - rectX / 2
    x' = fst windowSize / 2 - rectX

nextBlocks :: (Int, [Int]) -> Picture
nextBlocks (n, xs)
  | n == 1 = nextBlockFunc color1' color1 color1 ratio (1, nextBlockOffsetX)
  | n == 2 = nextBlockFunc color2' color2 color2 ratio (2, nextBlockOffsetX)
  | n == 3 = nextBlockFunc color3' color3 color3 ratio (3, nextBlockOffsetX)
  | otherwise = pictures $ map (nextBlockFunc color3' color3 colorThree ratio)
                               (zip xs $ map nextSpace [0..])
  where
    nextBlockFunc bColor tColor sColor r (i, x) =
      translate x 0 . scale r r $ blockFunc i bColor tColor sColor
    ratio = goldRatio * 0.9
    nextBlockOffsetX = negate $ fst windowSize / 2 - rectX + fst frameSize * ratio / 2
    nextBlockSpace = 5
    nextSpace n = nextBlockOffsetX - n * fst frameSize * ratio - n * nextBlockSpace 
