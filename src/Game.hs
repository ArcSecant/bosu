{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Game where

import Control.Arrow

import Lib

import Data.Maybe
import Data.List
import Data.Sequence ((><), (!?), Seq((:<|), (:|>), Empty))
import qualified Data.Map.Strict as Map
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

data ScoreValue = H300 | H100 | H50 | Miss | S100 | None deriving (Show)

type HitType = Map.Map (Float, Vector) (ScoreValue, Float)
type SliderPaths = Map.Map (Float, Vector) (Seq.Seq Vector)

data BOsu = Game
    { allObjs :: [HitObject]
    , remainingObjs :: [HitObject]
    , timing :: [TimingPoint]
    , elapsedTime :: Float
    , cursorLoc :: (Float, Float)
    , curSM :: Float
    , curObjsToHit :: [HitObject]
    , hitScores :: HitType
    , otherCircs :: [Picture]
    , sliderPaths :: SliderPaths
    }

toSeconds :: HitObject -> Float
toSeconds x = fromIntegral (time x) / 1000

getKey :: HitObject -> (Float, Vector)
getKey ho = (toSeconds ho, toFloatPair $ position ho)

sliderRate :: Float -> Float -> [TimingPoint] -> Float
sliderRate curTime sm tps' = let tps = reverse $ filter (\x -> beatLength x <= 0) tps' in
    sliderRate' tps
    where sliderRate' ts
            | null ts = 270 * sm
            | curTime * 1000 >= realToFrac (offset (head ts)) = 270 * (-100) * sm / realToFrac (beatLength (head ts))
            | otherwise = sliderRate' (tail ts)

parseFile :: FilePath -> T.Text -> (T.Text, BOsu)
parseFile path content = case parse beatmap path content of
    Left  e -> (T.pack "", Game {})
    Right b -> let sm = sliderMultiplier b in case sm of
        Nothing -> (fromJust $ audioFileName b, game)
        Just a -> (fromJust $ audioFileName b, game {curSM = realToFrac a})
        where 
            game = Game
                { allObjs = hitObjects b
                , remainingObjs = hitObjects b
                , timing = timingPoints b
                , elapsedTime = -0.25
                , curSM = 1.4
                , cursorLoc = (256, 192)
                , curObjsToHit = []
                , hitScores = Map.empty
                , otherCircs = []
                , sliderPaths = Map.map Seq.fromList $ getSliderPaths (hitObjects b) Map.empty
                }
            getSliderPaths [] acc = acc
            getSliderPaths (h:hs) acc = case details h of
                Slider shape info pLength -> case shape of
                    Linear points -> getSliderPaths hs $
                        Map.insert (toSeconds h, toFloatPair $ position h) (getBezierPath (realToFrac pLength) [toFloatPair (position h) : toVec points]) acc
                    Bezier points -> getSliderPaths hs $
                        Map.insert (toSeconds h, toFloatPair $ position h) (getBezierPath (realToFrac pLength) $ newHead : map toVec (tail points)) acc
                        where newHead = toFloatPair (position h) : toVec (head points)
                    Perfect a b -> getSliderPaths hs $
                        Map.insert (toSeconds h, toFloatPair $ position h) (getCirclePath (toFloatPair $ position h) (toFloatPair a) (toFloatPair b)) acc
                _ -> getSliderPaths hs acc

getVisibleObjs :: Float -> BOsu -> [HitObject]
getVisibleObjs curTime game = filter isVisible (allObjs game) where
    (sm, tps) = (curSM game, timing game)
    isVisible ho = case details ho of
        Spinner endtime -> toSeconds ho - 0.75 <= curTime && fromIntegral endtime / 1000 >= curTime
        HitCircle -> toSeconds ho - 0.75 <= curTime && toSeconds ho >= curTime
        Slider _ info pLength -> toSeconds ho - 0.75 <= curTime && toSeconds ho + addTime >= curTime
            where addTime = fromIntegral (repeats info) * realToFrac pLength / sliderRate (toSeconds ho) sm tps

renderHitObjects :: BOsu -> [Picture]
renderHitObjects game = let (sm, tps, hitObjs) = (curSM game, timing game, curObjsToHit game) in
    map renderHObj hitObjs
    where renderHObj ho = case details ho of
            HitCircle -> hCircle (position ho) 30
            Spinner endTime -> hCircle (256, 192) (300 * r)
                    where r = min 1 $ (fromIntegral endTime/1000 - elapsedTime game) / (fromIntegral endTime/1000 - toSeconds ho)
            Slider shape _ pLength -> case shape of
                Linear points -> drawBezier
                    (realToFrac pLength)
                    [map reCenter $ toFloatPair (position ho) : toVec points]
                Bezier points -> drawBezier (realToFrac pLength) $ map (map reCenter) $ newHead : map toVec (tail points)
                    where newHead = toFloatPair (position ho) : toVec (head points)
                Perfect a b -> drawArc (reCenter $ toFloatPair $ position ho) (reCenter $ toFloatPair a) (reCenter $ toFloatPair b)

makeApproachCirc :: BOsu -> [Picture]
makeApproachCirc game = map makeApproachCirc' (curObjsToHit game)
    where
        curTime = elapsedTime game  
        makeApproachCirc' ho = let r = max 1 (1.0 + 2.0 * (toSeconds ho - curTime) / 0.75) in
            case details ho of
                Spinner _ -> Blank
                _ -> if r == 1 then Blank else aCircle (position ho) (30.0 * r)

renderSliderball :: BOsu -> [Picture]
renderSliderball game = map renderSlider (curObjsToHit game) where
    (curTime, sm, tps) = (elapsedTime game, curSM game, timing game)
    renderSlider ho = case details ho of
        Slider shape info pLength -> case shape of
            Linear points -> renderSliderball' ho $ fromMaybe Seq.empty $ Map.lookup (getKey ho) $ sliderPaths game
            Bezier points -> renderSliderball' ho $ fromMaybe Seq.empty $ Map.lookup (getKey ho) $ sliderPaths game
            Perfect a b -> renderSliderball' ho $ fromMaybe Seq.empty $ Map.lookup (getKey ho) $ sliderPaths game
            where
                renderSliderball' ho path' = let
                    (curX, curY) = getSliderballPos game path' (Slider shape info pLength) (toSeconds ho)
                    sliderBall = Translate (curX-256) (curY-192) $ Color white $ ThickCircle 25 7.5
                    in if curTime > toSeconds ho then Pictures [sliderBall, Translate (curX - 256) (curY - 192) $ Color (greyN 0.75) $ ThickCircle 60 2.5] else sliderBall
        _ -> Blank

getSliderballPos :: BOsu -> Seq.Seq Vector -> HitObjectDetails -> Float -> Vector
getSliderballPos game path' (Slider shape info pLength) objStartTime = let
    (curTime, sm, tps) = (elapsedTime game, curSM game, timing game)
    path'' n =
        if even n then (path' >< Seq.reverse path') >>= Seq.replicate (n `div` 2)
        else path'' (n - 1) >< path'
    path = path'' $ repeats info
    lPath =  Seq.length path
    lastsFor = fromIntegral (repeats info) * (realToFrac pLength :: Float) / sliderRate objStartTime sm tps
    deltaT = lastsFor / fromIntegral lPath
    curIdx = let t = curTime - objStartTime in if t >= 0 then min (floor $ t/deltaT) (lPath - 1) else 0
    in fromMaybe (0, 0) (path !? curIdx)

renderHit :: ScoreValue -> Picture
renderHit ht = Scale 0.35 0.35 $ case ht of
    H300 -> Color (bright blue) $ text "300"
    H100 -> Color green $ text "100"
    H50 -> Color azure $ text "50"
    S100 -> Color (bright green) $ text "SLIDER"
    Miss -> Color red $ text "miss"
    None -> Blank

getHitFeedback :: BOsu -> Picture
getHitFeedback game = case remainingObjs game of
    [] -> Blank
    o:_ -> let
        key = getKey o
        (t, (x, y)) = key
        (scoreVal, hitTime) = fromMaybe (Miss, t) (Map.lookup key hScores)
        in case details o of
            HitCircle -> if curTime >= hitTime + 0.1 && hitTime + 0.2 > curTime then
                Translate (x - 256) (y - 192) $ renderHit scoreVal else
                Blank
            Slider _ info pLength -> if curTime >= hitTime + addTime && hitTime + addTime + 0.1 > curTime then
                case Map.lookup key (sliderPaths game) of
                    Nothing -> Blank
                    Just sliderPath -> let _ :|> (x, y) = sliderPath in Translate (x - 256) (y - 192) $ renderHit scoreVal else
                Blank
                where addTime = fromIntegral (repeats info) * realToFrac pLength / sliderRate (toSeconds o) sm tps
            Spinner endTime -> Blank
    where
        (curTime, hScores) = (elapsedTime game, hitScores game)
        (sm, tps) = (curSM game, timing game)

checkHit :: KeyState -> Vector -> BOsu -> HitType
checkHit keyState (x, y) game = if null $ remainingObjs game then hitScores game else let curObj = head (remainingObjs game) in
    case details curObj of
        HitCircle -> Map.insert (toSeconds curObj, toFloatPair $ position curObj) (getHit curObj, elapsedTime game) (hitScores game)
        Spinner endTime -> hitScores game
        sliderInfo -> case Map.lookup (getKey curObj) (sliderPaths game) of
            Nothing -> hitScores game
            Just sliderPath -> let pos = getSliderballPos game sliderPath sliderInfo (toSeconds curObj) in
                if keyState == Down && distSqr pos (x + 256, y + 192) <= 50 ** 2
                    then Map.insert (toSeconds curObj, toFloatPair $ position curObj) (S100, elapsedTime game) (hitScores game)
                    else hitScores game
    where
        -- Baby's first arrow
        -- magicMap f = map (fst &&& f . fst)
        mapInsert (a, b) = Map.insert a b
        curTime = elapsedTime game
        getHit ho
            | abs (curTime - toSeconds ho) < 0.05 && isInCircle ho = H300
            | abs (curTime - toSeconds ho) < 0.1 && isInCircle ho = H100
            | abs (curTime - toSeconds ho) < 0.2 && isInCircle ho = H50
            | otherwise = Miss
        isInCircle ho = distSqr (toFloatPair $ position ho) (x + 256, y + 192) <= 50 ** 2

handleInput :: Event -> BOsu -> BOsu
handleInput (EventMotion pos) game = game {cursorLoc = pos}
handleInput (EventKey (Char c) state _ pos) game = if c == 'z' || c == 'x'
    then game {hitScores = checkHit state pos game}
    else game
handleInput _ game = game

render :: BOsu -> Picture
render game = Pictures (hitCircles ++ sliderball ++ [cursor, hitFeedback])
    where
        cursor = let (x, y) = cursorLoc game in Translate x y $ Color yellow $ circleSolid 10
        hitFeedback = getHitFeedback game
        hitCircles = renderHitObjects game
        sliderball = otherCircs game

update :: Float -> BOsu -> BOsu
update tDelta game = let
    curTime = elapsedTime game + tDelta
    game' = game
        { elapsedTime = curTime
        , curObjsToHit = getVisibleObjs curTime game }
    approachCircs = makeApproachCirc game'
    sliderball = renderSliderball game'
    newRemObjs = let remObjs = remainingObjs game in
        case remObjs of
            [] -> []
            r:rs -> case details r of
                HitCircle -> if curTime >= toSeconds (head remObjs) + 0.2 then rs else remObjs
                Slider shape info pLength -> if curTime >= toSeconds (head remObjs) + sliderDuration + 0.2 then rs else remObjs
                    where
                        sliderDuration = fromIntegral (repeats info) * realToFrac pLength / sliderRate (toSeconds r) sm tps
                        (sm, tps) = (curSM game, timing game)
                -- Spinner endTime -> if curTime <= fromIntegral endTime then rs else remObjs
                _ -> rs
    in game' {remainingObjs = newRemObjs, otherCircs = approachCircs ++ sliderball}