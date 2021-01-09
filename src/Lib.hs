{-# LANGUAGE DataKinds #-}

module Lib where

import Data.List
import Control.Lens
import Data.Geometry hiding (Vector, head)
import Graphics.Gloss
import Graphics.Gloss.Data.Vector

import qualified Data.Sequence as Seq
import qualified Data.Geometry.BezierSpline as BS

-- placePoints :: Int -> [Float] -> [Float]
-- placePoints m pts = map f [0..m]
--     where f r = let
--             d = realToFrac (fromIntegral r)/(m-1)
--             i = case findIndex (> d) pts of
--                 Just a -> a
--                 Nothing -> 0
--             u = (d - pts !! i)/(pts !! (i + 1) - pts !! (i - 1))
--             t = ((fromIntegral i :: Float) + u)/(fromIntegral $ length pts - 1)
--             in pts !! (round t)

makeBezier :: [Vector] -> BS.BezierSpline 1 2 Float
makeBezier xs = BS.fromPointSeq $ Seq.fromList $ map (\(x, y) -> Point2 x y) xs

getBezierPath :: Float -> [Vector] -> [Vector]
-- getBezierPath xs = map (\p -> (p ^. xCoord, p ^. yCoord)) $ BS.approximate 0.5 (makeBezier xs)
getBezierPath l xs = map ((\p -> (p ^. xCoord, p ^. yCoord)) . BS.evaluate (makeBezier xs)) [0, min 0.1 (d/10/l) .. 1]
    where d = dist (head xs) (last xs)

dist :: Vector -> Vector -> Float
dist (a1, b1) (a2, b2) = sqrt $ (a2 - a1) ** 2 + (b2 - b1) ** 2

circumcircle :: Vector -> Vector -> Vector -> (Vector, Float)
circumcircle (x1, y1) (x2, y2) (x3, y3) = let
    d = 2 * (x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2))
    ux = ((x1 * x1 + y1 * y1) * (y2 - y3) + (x2 * x2 + y2 * y2) * (y3 - y1) + (x3 * x3 + y3 * y3) * (y1 - y2)) / d
    uy = ((x1 * x1 + y1 * y1) * (x3 - x2) + (x2 * x2 + y2 * y2) * (x1 - x3) + (x3 * x3 + y3 * y3) * (x2 - x1)) / d
    center = (ux, uy)
    in (center, dist center (x1, y1))

reCenter :: Vector -> Vector
reCenter (a, b) = (a - 256, b - 192)

hCircle :: (Int, Int) -> Float -> Picture
hCircle (x, y) r = Translate (fromIntegral x - 256) (fromIntegral y - 192) (Color cyan $ ThickCircle (r - 5) 7.5)

aCircle :: (Int, Int) -> Float -> Picture
aCircle (x, y) r = Translate (fromIntegral x - 256) (fromIntegral y - 192) $ Color white $ ThickCircle r 2.5

lineToCirc :: Vector -> Picture
lineToCirc (a, b) = Translate a b (Color cyan $ circleSolid 40)

drawBezier :: Float -> [Vector] -> Picture
drawBezier l ps = Color cyan $ Pictures $ map lineToCirc $ getBezierPath l ps
-- drawBezier :: Bezier -> Picture
-- drawBezier ps = Pictures [Color cyan $ Line $ map f $ getBezierPath ps, Color cyan $ Line $ map g $ getBezierPath ps]
--     where
--         f (a, b) = (a - 40, b - 40)
--         g (a, b) = (a + 40, b + 40)


-- test = do
--     display
--         (InWindow "Bézier" (600,600) (100,100))
--         white
--         (drawBezier [(0,0),(200,0)])