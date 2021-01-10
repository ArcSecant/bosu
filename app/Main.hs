{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Data.List
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified SDL
import qualified SDL.Mixer as Mixer

import Obmapp.Beatmap
import qualified Obmapp.Beatmap.General as G
import Obmapp.Parser.Beatmap
import Obmapp.Parser.Osu (versionInfo)
import Text.Megaparsec

import Debug.Trace
import Graphics.Gloss

import Graphics.Gloss.Interface.IO.Animate

import Lib

data VHO = Vector HitObject
type BInfo = (Float, [TimingPoint], [HitObject])

audioPath = "C:\\Program Files (x86)\\osu!\\Songs\\1199834 Vickeblanka - Black Rover (TV Size)\\audio.mp3"
mapPath = "C:\\Program Files (x86)\\osu!\\Songs\\1199834 Vickeblanka - Black Rover (TV Size)\\Vickeblanka - Black Rover (TV Size) (IOException) [Special].osu"

audioPath' = "C:\\Program Files (x86)\\osu!\\Songs\\437683 Halozy - Kikoku Doukoku Jigokuraku\\Kikoku Doukoku Jigokuraku.mp3"
mapPath' = "C:\\Program Files (x86)\\osu!\\Songs\\437683 Halozy - Kikoku Doukoku Jigokuraku\\Halozy - Kikoku Doukoku Jigokuraku (Hollow Wings) [Notch Hell].osu"

sliderRate :: Float -> Float -> [TimingPoint] -> Float
sliderRate curTime sm tps' = let tps = reverse $ filter (\x -> beatLength x <= 0) tps' in
    sliderRate' tps
    where sliderRate' ts = if null ts then 270*sm else if curTime*1000 >= (realToFrac $ offset (head ts)) then 270*(-100)*sm/(realToFrac $ beatLength (head ts)) else sliderRate' (tail ts)

getSec :: HitObject -> Float
getSec x = (fromIntegral $ time x) / 1000

parseFile :: FilePath -> T.Text -> BInfo
parseFile path content = case parse beatmap path content of
    Left  e -> (0, [], [])
    Right b -> let sm = sliderMultiplier b in case sm of
        Nothing -> (1.4, timingPoints b, hitObjects b)
        Just a -> (realToFrac a, timingPoints b, hitObjects b)

windowDisplay :: Display
windowDisplay = InWindow "BOsu" (1080, 720) (50, 50)

toFloatPair :: (Int, Int) -> Vector
toFloatPair (a, b) = (fromIntegral a, fromIntegral b)

toVec :: [Obmapp.Beatmap.Point] -> [Vector]
toVec xs = map toFloatPair xs

getHitObjects :: Float -> BInfo -> [Picture]
getHitObjects t (sm, tps, hitObjs) = map renderHObj hitObjs where
    renderHObj ho = case details ho of
        HitCircle -> hCircle (position ho) 30
        Spinner _ -> hCircle (256, 192) 300
        Slider shape _ pLength -> case shape of
            Linear points -> drawBezier (realToFrac pLength) $ [map reCenter $ (toFloatPair $ position ho):(toVec points)]
            Bezier points -> drawBezier (realToFrac pLength) $ map (map reCenter) $ [newHead] ++ (map toVec $ tail points)
                where newHead = (toFloatPair $ position ho):(toVec $ head points)
            Perfect a b -> drawArc (reCenter $ toFloatPair $ position ho) (reCenter $ toFloatPair a) (reCenter $ toFloatPair b)

getVisibleObjs :: Float -> BInfo -> [HitObject]
getVisibleObjs curTime (sm, tps, hitObjs) = filter isVisible hitObjs where
    isVisible ho = case details ho of
        Spinner endtime -> getSec ho - 0.5 <= curTime + 0.5 && fromIntegral endtime / 1000 >= curTime
        HitCircle -> getSec ho - 0.5 <= curTime && getSec ho >= curTime
        Slider _ info pLength -> getSec ho - 0.5 <= curTime && getSec ho + (fromIntegral $ repeats info)*(realToFrac pLength)/(sliderRate (getSec ho) sm tps) >= curTime

getSliderball :: Float -> BInfo -> [Picture]
getSliderball curTime (sm, tps, hitObjs) = map renderSlider hitObjs where
    renderSlider ho = case details ho of
        Slider shape info pLength -> case shape of
            Linear points -> makeSliderball ho $ getPathBezier (realToFrac pLength) $ [(toFloatPair $ position ho):(toVec points)]
            Bezier points -> makeSliderball ho $ getPathBezier (realToFrac pLength) $ [newHead] ++ (map toVec $ tail points)
                where newHead = (toFloatPair $ position ho):(toVec $ head points)
            Perfect a b -> makeSliderball ho $ getPathPerfect (toFloatPair $ position ho) (toFloatPair a) (toFloatPair b)
            where
                getPathBezier l ps = getBezierPath l ps
                getPathPerfect a b c = getCirclePath a b c
                makeSliderball ho path' = let
                    path'' n =
                        if even n then concat $ take (n `div` 2) $ repeat $ path' ++ (reverse path')
                        else (path'' (n-1)) ++ path'
                    path = V.fromList $ path'' $ repeats info
                    lPath = V.length $ path
                    lastsFor = (fromIntegral $ repeats info)*(realToFrac pLength :: Float) / (sliderRate curTime sm tps)
                    deltaT = lastsFor / (fromIntegral lPath)
                    curIdx = let t = curTime - getSec ho in if t >= 0 then min (floor $ t/deltaT) (lPath - 1) else 0
                    (curX, curY) = case path V.!? curIdx of
                        Just e -> e
                        Nothing -> (0,0)
                    sliderBall = Translate (curX-256) (curY-192) $ Color white $ ThickCircle 25 7.5
                    in case (curTime > getSec ho) of
                        True -> Pictures [sliderBall, Translate (curX-256) (curY-192) $ Color (greyN 0.75) $ ThickCircle 60 2.5]
                        _ -> sliderBall
        _ -> Blank
    
-- reCenter :: HitObject -> HitObject
-- reCenter p@(HitObject {position = (x, y)}) = p {position = (x - 256, y - 192)}

animationFunc :: BInfo -> Float -> Picture
animationFunc binfo t = Pictures (hitCircles ++ approachCircs ++ sliderBall)
    where
        (sm, tps, hitObjs) = binfo 
        visibleObjs = getVisibleObjs t binfo
        sliderBall = getSliderball t (sm, tps, visibleObjs)
        hitCircles = getHitObjects t (sm, tps, visibleObjs)
        approachCircs = map (getApprachCirc) visibleObjs
        getApprachCirc ho = let r = (max 1 (1.0 + 2.0 * (getSec ho - t) / 0.5)) in
            case details ho of
                Spinner _ -> Blank
                _ -> if r == 1 then Blank else aCircle (position ho) (30.0 * r)

test = do
    let (a, b, c) = ((100,-100), (200, 0), (100, 100)) in
        display windowDisplay black $ Pictures [hCircle (256, 192) 300, Color red $ Line $ getCirclePath a b c, Color white $ Line [a, b, c]]
-- $ getCirclePath (1, 1) (0, 2) (-1, 1)

main :: IO ()
main = do
    let (mp, ap) = (mapPath', audioPath')
    SDL.initialize [SDL.InitAudio]
    _ <- Mixer.openAudio Mixer.defaultAudio 4096
    audio <- Mixer.load ap
    _ <- Mixer.play audio
    file <- T.readFile mp
    let curBMap = parseFile mp file in
        animateIO windowDisplay black (\t -> return $ animationFunc curBMap t) (\_ -> return ())
