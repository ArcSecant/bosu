{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}

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
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Simulate

import Lib

data VHO = Vector HitObject
type BInfo = (Float, [TimingPoint], [HitObject])

data HitType = H300 | H100 | H50 | Miss | None deriving (Show)

data BOsu = Game
    -- Static
    { allObjs :: [HitObject]
    , timing :: [TimingPoint]
    -- Changes
    , elapsedTime :: Float
    , cursorLoc :: (Float, Float)
    , curSM :: Float
    , curObjsToHit :: [HitObject]
    , otherCircs :: [Picture]
    , hitMiss :: (Float, Vector, HitType)
    }

windowDisplay :: Display
windowDisplay = InWindow "BOsu" (1080, 720) (50, 50)

audioPath = "C:\\Program Files (x86)\\osu!\\Songs\\1199834 Vickeblanka - Black Rover (TV Size)\\audio.mp3"
mapPath = "C:\\Program Files (x86)\\osu!\\Songs\\1199834 Vickeblanka - Black Rover (TV Size)\\Vickeblanka - Black Rover (TV Size) (IOException) [Special].osu"

audioPath' = "C:\\Program Files (x86)\\osu!\\Songs\\437683 Halozy - Kikoku Doukoku Jigokuraku\\Kikoku Doukoku Jigokuraku.mp3"
mapPath' = "C:\\Program Files (x86)\\osu!\\Songs\\437683 Halozy - Kikoku Doukoku Jigokuraku\\Halozy - Kikoku Doukoku Jigokuraku (Hollow Wings) [Notch Hell].osu"

getSec :: HitObject -> Float
getSec x = (fromIntegral $ time x) / 1000

sliderRate :: Float -> Float -> [TimingPoint] -> Float
sliderRate curTime sm tps' = let tps = reverse $ filter (\x -> beatLength x <= 0) tps' in
    sliderRate' tps
    where sliderRate' ts = if null ts then 330*sm else if curTime*1000 >= (realToFrac $ offset (head ts)) then 330*(-100)*sm/(realToFrac $ beatLength (head ts)) else sliderRate' (tail ts)

parseFile :: FilePath -> T.Text -> BOsu
parseFile path content = case parse beatmap path content of
    Left  e -> Game {}
    Right b -> let sm = sliderMultiplier b in case sm of
        Nothing -> game
        Just a -> game {curSM = realToFrac a}
        where game = Game
                { allObjs = hitObjects b
                , timing = timingPoints b
                , elapsedTime = -0.25
                , curSM = 1.4
                , cursorLoc = (256, 192)
                , curObjsToHit = []
                , otherCircs = []
                , hitMiss = (0, (0,0), None)
                }

getVisibleObjs :: Float -> BOsu -> [HitObject]
getVisibleObjs curTime game = filter isVisible (allObjs game) where
    (sm, tps) = (curSM game, timing game)
    isVisible ho = case details ho of
        Spinner endtime -> getSec ho - 0.5 <= curTime + 0.5 && fromIntegral endtime / 1000 >= curTime
        HitCircle -> getSec ho - 0.5 <= curTime && getSec ho >= curTime
        Slider _ info pLength -> getSec ho - 0.5 <= curTime && getSec ho + addTime >= curTime
            where addTime = (fromIntegral $ repeats info)*(realToFrac pLength)/(sliderRate (getSec ho) sm tps)

renderHitObjects :: BOsu -> [Picture]
renderHitObjects game = let (sm, tps, hitObjs) = (curSM game, timing game, curObjsToHit game) in map renderHObj hitObjs where
    renderHObj ho = case details ho of
        HitCircle -> hCircle (position ho) 30
        Spinner _ -> hCircle (256, 192) 300
        Slider shape _ pLength -> case shape of
            Linear points -> drawBezier (realToFrac pLength) $ [map reCenter $ (toFloatPair $ position ho):(toVec points)]
            Bezier points -> drawBezier (realToFrac pLength) $ map (map reCenter) $ [newHead] ++ (map toVec $ tail points)
                where newHead = (toFloatPair $ position ho):(toVec $ head points)
            Perfect a b -> drawArc (reCenter $ toFloatPair $ position ho) (reCenter $ toFloatPair a) (reCenter $ toFloatPair b)

makeSliderball :: Float -> BOsu -> [Picture]
makeSliderball curTime game = map renderSlider (curObjsToHit game) where
    (sm, tps) = (curSM game, timing game)
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
                    lastsFor = (fromIntegral $ repeats info)*(realToFrac pLength :: Float) / (sliderRate (getSec ho) sm tps)
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

makeApproachCirc :: Float -> BOsu -> [Picture]
makeApproachCirc curTime game = map makeApproachCirc' (curObjsToHit game)
    where makeApproachCirc' ho = let r = (max 1 (1.0 + 2.0 * (getSec ho - curTime) / 0.5)) in
            case details ho of
                Spinner _ -> Blank
                _ -> if r == 1 then Blank else aCircle (position ho) (30.0 * r)

renderHit :: HitType -> Picture
renderHit ht = case ht of
    H300 -> Color (bright blue) $ text "300"
    H100 -> Color green $ text "100"
    H50 -> Color azure $ text "50"
    Miss -> Color red $ text "miss"
    None -> Blank

checkHit :: Vector -> BOsu -> HitType
checkHit (x, y) game = let curObjs = curObjsToHit game in 
    case map getHit curObjs of
        [] -> None
        x:xs -> x
    where
        curTime = elapsedTime game
        getHit ho
            | abs (curTime - getSec ho) < 0.05 && isInCircle ho = H300
            | abs (curTime - getSec ho) < 0.1 && isInCircle ho = H100
            | abs (curTime - getSec ho) < 0.2 && isInCircle ho = H50
            | abs (curTime - getSec ho) < 0.4 = Miss
            | otherwise = None
        isInCircle ho = distSqr (toFloatPair $ position ho) (x + 256, y + 192) <= 40 ** 2

handleInput :: Event -> BOsu -> BOsu
handleInput (EventMotion pos) game = game {cursorLoc = pos, hitMiss = (elapsedTime game, (0,0), None)}
handleInput (EventKey (Char c) _ _ pos) game = if c == 'z' || c == 'x'
    then game {hitMiss = (elapsedTime game, pos, checkHit pos game)}
    else game
handleInput _ game = game

render :: BOsu -> Picture
render game = Pictures (hitCircles ++ sliderball ++ [cursor, hit])
    where
        cursor = let (x, y) = cursorLoc game in Translate x y $ Color yellow $ circleSolid 10
        hit = let (hitTime, (x, y), ht) = traceShow (hitMiss game) (hitMiss game) in if elapsedTime game >= hitTime + 0.25 then Blank else
            Translate x y $ Scale 0.5 0.5 $ renderHit ht
        hitCircles = renderHitObjects game
        sliderball = otherCircs game

update :: Float -> BOsu -> BOsu
update tDelta game = let
    curTime = elapsedTime game + tDelta
    game' = game
        { elapsedTime = curTime
        , curObjsToHit = getVisibleObjs curTime game }
    approachCircs = makeApproachCirc curTime game'
    sliderball = makeSliderball curTime game'
    in game' {otherCircs = approachCircs ++ sliderball}

main :: IO ()
main = do
    let (mp, ap) = (mapPath', audioPath')
    SDL.initialize [SDL.InitAudio]
    _ <- Mixer.openAudio Mixer.defaultAudio 4096
    audio <- Mixer.load ap
    _ <- Mixer.play audio
    file <- T.readFile mp
    let curBMap = parseFile mp file in
        play windowDisplay black 120 curBMap render handleInput update
