{-# LANGUAGE DataKinds #-}

module Lib where

import Data.List
import Control.Lens
import Data.Ext
import Data.Geometry hiding (Vector, head)
import Data.Geometry.PolyLine
import Graphics.Gloss
import Graphics.Gloss.Data.Vector

import qualified Data.Sequence as Seq
import qualified Data.Geometry.BezierSpline as BS

distSqr :: Vector -> Vector -> Float
distSqr (a1, b1) (a2, b2) = (a2 - a1) ** 2 + (b2 - b1) ** 2

dist :: Vector -> Vector -> Float
dist a b = sqrt $ distSqr a b

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
hCircle (x, y) r = Translate (fromIntegral x - 256) (fromIntegral y - 192) (Color cyan $ ThickCircle (r - 2.5) 5)

aCircle :: (Int, Int) -> Float -> Picture
aCircle (x, y) r = Translate (fromIntegral x - 256) (fromIntegral y - 192) $ Color white $ ThickCircle r 2.5

lineToCirc :: Vector -> Picture
lineToCirc (a, b) = Translate a b (Color cyan $ circleSolid 30)

makeBezier :: [[Vector]] -> [BS.BezierSpline 1 2 Float]
makeBezier xxs = map (BS.fromPointSeq . Seq.fromList . map (\(x, y) -> Point2 x y)) xxs

getBezierPath :: Float -> [[Vector]] -> [Vector]
getBezierPath pLength xxs = case fromPoints (map ext bPath) of
    Just path -> map ((\p -> (p ^. xCoord, p ^. yCoord)) . (\x -> interpolatePoly x path)) [0..(fromIntegral $ length bPath)]
    -- [0,(pLength/150)..(fromIntegral $ length bPath)]
    Nothing -> []
    where
        bPath = concat $ map (\x -> map (BS.evaluate x) [0,0.02..1]) $ makeBezier xxs

getCirclePath :: Vector -> Vector -> Vector -> [Vector]
getCirclePath a b c = let
    (center , r) = circumcircle a b c 
    (cX, cY) = center
    (cpx, cpy) = (fst a - cX, snd a - cY)
    angle = acos((distSqr a center + distSqr center c - distSqr a c)/(2 * dist a center * dist center c))
    nodes = map (\x -> argV (cpx, cpy) + x * angle / 50) [0 .. 50]
    xs = map (\x -> cX + r * cos x) nodes
    ys = map (\x -> cY + r * sin x) nodes
    in zip xs ys

drawArc :: Vector -> Vector -> Vector  -> Picture
drawArc a b c = Color cyan $ Pictures $ map lineToCirc $ getCirclePath a b c

drawBezier :: Float -> [[Vector]] -> Picture
drawBezier l ps = Color cyan $ Pictures $ map lineToCirc $ getBezierPath l ps
