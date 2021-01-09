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

import Lib

data VHO = Vector HitObject
type BInfo = (Float, [TimingPoint], [HitObject])

audioPath = "C:\\Program Files (x86)\\osu!\\Songs\\1199834 Vickeblanka - Black Rover (TV Size)\\audio.mp3"
mapPath = "C:\\Program Files (x86)\\osu!\\Songs\\1199834 Vickeblanka - Black Rover (TV Size)\\Vickeblanka - Black Rover (TV Size) (IOException) [Special].osu"

audioPath' = "C:\\Program Files (x86)\\osu!\\Songs\\903700 Co shu Nie - asura\\audio.mp3"
mapPath' = "C:\\Program Files (x86)\\osu!\\Songs\\903700 Co shu Nie - asura\\Co shu Nie - Asura (ailv) [Lament].osu"

sliderRate :: Float -> Float -> [TimingPoint] -> Float
sliderRate curTime sm tps' = let tps = reverse $ filter (\x -> beatLength x <= 0) tps' in
    sliderRate' tps
    where sliderRate' ts = if null ts then 340*sm else if curTime*1000 >= (realToFrac $ offset (head ts)) then 340*(-100)*sm/(realToFrac $ beatLength (head ts)) else sliderRate' (tail ts)

getSec :: HitObject -> Float
getSec x = (fromIntegral $ time x) / 1000

parseFile :: FilePath -> T.Text -> BInfo
parseFile path content = case parse beatmap path content of
    Left  e -> (0, [], [])
    Right b -> let sm = sliderMultiplier b in case sm of
        Nothing -> (1.4, timingPoints b, hitObjects b)
        Just a -> (realToFrac a, timingPoints b, hitObjects b)

windowDisplay :: Display
windowDisplay = InWindow "BOsu" (1080, 720) (0, 0)

toFloatPair :: (Int, Int) -> Vector
toFloatPair (a, b) = (fromIntegral a, fromIntegral b)

toVec :: [Obmapp.Beatmap.Point] -> [Vector]
toVec xs = map toFloatPair xs

getHitObjects :: Float -> BInfo -> [Picture]
getHitObjects t (sm, tps, hitObjs) = map renderHCircle hitObjs where
    renderHCircle ho = case details ho of
        HitCircle -> hCircle (position ho) 40
        Spinner _ -> hCircle (256, 192) 300
        Slider shape _ pLength -> case shape of
            Linear points -> drawBezier (realToFrac pLength) $ map reCenter $ (toFloatPair $ position ho):(toVec points)
            Bezier points -> Pictures $ map (drawBezier (realToFrac $ pLength/(fromIntegral $ length points)) . map reCenter) $ [newHead] ++ (map toVec $ tail points)
                where newHead = (toFloatPair $ position ho):(toVec $ head points)
            Perfect a b -> drawBezier (realToFrac pLength) $ map reCenter $ (toFloatPair $ position ho):(toVec [a, b])
            -- Perfect a b -> let ((x, y), r) = circumcircle (toFloatPair $ position ho) (toFloatPair a) (toFloatPair b) in
            --     Translate (x - 256) (y - 192) (Color cyan $ ThickCircle r 80)
            _ -> hCircle (position ho) 40
        where (a, b) = toFloatPair $ position ho

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
            Linear points -> makeSliderball 1 ho $ (toFloatPair $ position ho):(toVec points)
            Bezier points -> makeSliderball (length points) ho $ concat $ map (getBezierPath (realToFrac pLength)) $ [newHead] ++ (map toVec $ tail points)
                where newHead = (toFloatPair $ position ho):(toVec $ head points)
            Perfect a b -> makeSliderball 1 ho $ (toFloatPair $ position ho):(toVec [a, b])
            where
                makeSliderball l ho ps = let
                    path' = getBezierPath (realToFrac $ pLength/fromIntegral l) ps
                    path'' n =
                        if even n then concat $ take (n `div` 2) $ repeat $ path' ++ (reverse path')
                        else (path'' (n-1)) ++ path'
                    path = V.fromList $ map head $ group $ path'' $ repeats info
                    lPath = V.length path
                    lastsFor = (fromIntegral $ repeats info)*(realToFrac pLength :: Float) / (sliderRate curTime sm tps)
                    deltaT = lastsFor / (fromIntegral lPath)
                    curIdx = let t = curTime - getSec ho in if t >= 0 then min (floor $ t/deltaT) (lPath - 1) else 0
                    (curX, curY) = path V.! curIdx
                    in Translate (curX-256) (curY-192) $ Color white $ ThickCircle 35 7.5
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
                _ -> if r == 1 then Blank else aCircle (position ho) (40.0 * r)

main :: IO ()
main = do
    SDL.initialize [SDL.InitAudio]
    _ <- Mixer.openAudio Mixer.defaultAudio 4096
    audio <- Mixer.load audioPath'
    _ <- Mixer.play audio
    file <- T.readFile mapPath'
    let curBMap = parseFile mapPath' file in
        animate windowDisplay black $ animationFunc curBMap
