module Lib where

import Graphics.Gloss
import Graphics.Gloss.Data.Vector

type Bezier = [(Float, Float)]

circumcircle :: Vector -> Vector -> Vector -> (Vector, Float)
circumcircle (x1, y1) (x2, y2) (x3, y3) = let
    d = 2 * (x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2))
    ux = ((x1 * x1 + y1 * y1) * (y2 - y3) + (x2 * x2 + y2 * y2) * (y3 - y1) + (x3 * x3 + y3 * y3) * (y1 - y2)) / d
    uy = ((x1 * x1 + y1 * y1) * (x3 - x2) + (x2 * x2 + y2 * y2) * (x1 - x3) + (x3 * x3 + y3 * y3) * (x2 - x1)) / d
    center = (ux, uy)
    in (center, dist center (x1, y1))
    where
        dist (a1, b1) (a2, b2) = sqrt $ (a2 - a1) ** 2 + (b2 - b1) ** 2

reCenter :: (Float, Float) -> (Float, Float)
reCenter (a, b) = (a - 256, b - 192)

addV :: Vector -> Vector -> Vector
addV (a, b) (c, d) = (a + c, b + d)

subV :: Vector -> Vector -> Vector
subV (a,b) (c,d) = (a - c,b - d)

makePairs :: [a] -> [(a, a)]
makePairs ps = let
    ps' = take (length ps - 1) ps
    ps'' = drop 1 ps
    in zip ps' ps''

listify :: (a, a) -> [a]
listify (a, b) = [a, b]

bezier :: Float -> Bezier -> Vector
bezier u (p1:p2:[]) = addV p1 $ mulSV u (subV p2 p1)
bezier u ps         = let pairs = map listify $ makePairs ps in bezier u $ (map (bezier u)) pairs

hCircle :: (Int, Int) -> Float -> Picture
hCircle (x, y) r = Translate (fromIntegral x - 256) (fromIntegral y - 192) (Color cyan $ ThickCircle (r - 5) 7.5)

aCircle :: (Int, Int) -> Float -> Picture
aCircle (x, y) r = Translate (fromIntegral x - 256) (fromIntegral y - 192) $ Color white $ ThickCircle r 2.5

lineToCirc :: Vector -> Picture
lineToCirc (a, b) = Translate a b (Color cyan $ circleSolid 40)

getBezierPath :: Bezier -> [Vector]
getBezierPath ps = let
    p1 = head ps
    pn = last ps
    nsteps = (magV $ subV pn p1) / 10
    in map (\u -> bezier u ps) [0, (min 0.1 (1/nsteps)) .. 1]

drawBezier :: Bezier -> Picture
drawBezier ps = Color cyan $ Pictures $ map lineToCirc $ getBezierPath ps
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