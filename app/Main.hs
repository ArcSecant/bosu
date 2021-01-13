{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Control.Arrow

import Data.Maybe
import Data.List
import Data.Vector ((!?))
import Data.Sequence (Seq((:<|), Empty))
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

import System.Environment (getArgs)
import System.Directory.Extra (listFiles)

import Lib

data ScoreValue = H300 | H100 | H50 | Miss | None deriving (Show)

type HitType = Map.Map (Float, Vector) ScoreValue

data BOsu = Game
    -- Static
    { allObjs :: [HitObject]
    , timing :: [TimingPoint]
    -- Changes
    , elapsedTime :: Float
    , cursorLoc :: (Float, Float)
    , curSM :: Float
    , curObjsToHit :: [HitObject]
    , hitScores :: HitType
    , otherCircs :: [Picture]
    }

windowDisplay :: Display
windowDisplay = InWindow "BOsu" (1080, 720) (50, 50)

getSec :: HitObject -> Float
getSec x = fromIntegral (time x) / 1000

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
        where game = Game
                { allObjs = hitObjects b
                , timing = timingPoints b
                , elapsedTime = -0.25
                , curSM = 1.4
                , cursorLoc = (256, 192)
                , curObjsToHit = []
                , hitScores = Map.empty
                , otherCircs = []
                }

getVisibleObjs :: Float -> BOsu -> [HitObject]
getVisibleObjs curTime game = filter isVisible (allObjs game) where
    (sm, tps) = (curSM game, timing game)
    isVisible ho = case details ho of
        Spinner endtime -> getSec ho - 0.5 <= curTime + 0.5 && fromIntegral endtime / 1000 >= curTime
        HitCircle -> getSec ho - 0.5 <= curTime && getSec ho >= curTime
        Slider _ info pLength -> getSec ho - 0.5 <= curTime && getSec ho + addTime >= curTime
            where addTime = fromIntegral (repeats info) * realToFrac pLength/sliderRate (getSec ho) sm tps

renderHitObjects :: BOsu -> [Picture]
renderHitObjects game = let (sm, tps, hitObjs) = (curSM game, timing game, curObjsToHit game) in map renderHObj hitObjs where
    renderHObj ho = case details ho of
        HitCircle -> hCircle (position ho) 30
        Spinner _ -> hCircle (256, 192) 300
        Slider shape _ pLength -> case shape of
            Linear points -> drawBezier
                (realToFrac pLength)
                [map reCenter $ toFloatPair (position ho) : toVec points]
            Bezier points -> drawBezier (realToFrac pLength) $ map (map reCenter) $ newHead : map toVec (tail points)
                where newHead = toFloatPair (position ho) : toVec (head points)
            Perfect a b -> drawArc (reCenter $ toFloatPair $ position ho) (reCenter $ toFloatPair a) (reCenter $ toFloatPair b)

makeSliderball :: BOsu -> [Picture]
makeSliderball game = map renderSlider (curObjsToHit game) where
    (curTime, sm, tps) = (elapsedTime game, curSM game, timing game)
    renderSlider ho = case details ho of
        Slider shape info pLength -> case shape of
            Linear points -> makeSliderball ho $ getBezierPath (realToFrac pLength) [toFloatPair (position ho) : toVec points]
            Bezier points -> makeSliderball ho $ getBezierPath (realToFrac pLength) $ newHead : map toVec (tail points)
                where newHead = toFloatPair (position ho) : toVec (head points)
            Perfect a b -> makeSliderball ho $ getCirclePath (toFloatPair $ position ho) (toFloatPair a) (toFloatPair b)
            where
                makeSliderball ho path' = let
                    path'' n =
                        if even n then concat $ replicate (n `div` 2) (path' ++ reverse path')
                        else path'' (n - 1) ++ path'
                    path = V.fromList $ path'' $ repeats info
                    lPath = V.length path
                    lastsFor = fromIntegral (repeats info) * (realToFrac pLength :: Float) / sliderRate (getSec ho) sm tps
                    deltaT = lastsFor / fromIntegral lPath
                    curIdx = let t = curTime - getSec ho in if t >= 0 then min (floor $ t/deltaT) (lPath - 1) else 0
                    (curX, curY) = fromMaybe (0, 0) (path !? curIdx)
                    sliderBall = Translate (curX-256) (curY-192) $ Color white $ ThickCircle 25 7.5
                    in if curTime > getSec ho then Pictures [sliderBall, Translate (curX - 256) (curY - 192) $ Color (greyN 0.75) $ ThickCircle 60 2.5] else sliderBall
        _ -> Blank

makeApproachCirc :: BOsu -> [Picture]
makeApproachCirc game = map makeApproachCirc' $ curObjsToHit game
    where
        curTime = elapsedTime game  
        makeApproachCirc' ho = let r = max 1 (1.0 + 2.0 * (getSec ho - curTime) / 0.5) in
            case details ho of
                Spinner _ -> Blank
                _ -> if r == 1 then Blank else aCircle (position ho) (30.0 * r)

renderHit :: ScoreValue -> Picture
renderHit ht = Scale 0.35 0.35 $ case ht of
    H300 -> Color (bright blue) $ text "300"
    H100 -> Color green $ text "100"
    H50 -> Color azure $ text "50"
    Miss -> Color red $ text "miss"
    None -> Blank

checkHit :: Vector -> BOsu -> HitType
checkHit (x, y) game = let curObjs = curObjsToHit game in 
    foldr mapInsert Map.empty $ zip (map (\x -> (getSec x, toFloatPair $ position x)) curObjs) (map getHit curObjs)
    where
        -- Baby's first arrow
        -- magicMap f = map (fst &&& f . fst)
        mapInsert (a, b) = Map.insert a b
        curTime = elapsedTime game
        getHit ho
            | abs (curTime - getSec ho) < 0.05 && isInCircle ho = H300
            | abs (curTime - getSec ho) < 0.1 && isInCircle ho = H100
            | abs (curTime - getSec ho) < 0.2 && isInCircle ho = H50
            | otherwise = Miss
        isInCircle ho = distSqr (toFloatPair $ position ho) (x + 256, y + 192) <= 40 ** 2

getHitFeedback :: BOsu -> Picture
getHitFeedback game = case filter f objs of
    [] -> Blank
    ho:_ -> let
        key = (getSec ho, toFloatPair $ position ho)
        (_, (x, y)) = key
        in case Map.lookup key hScores of
            Nothing -> Translate (x - 256) (y - 192) $ renderHit Miss
            Just hValue -> Translate (x - 260) (y - 195) $ renderHit hValue
    where
        (curTime, objs, hScores) = (elapsedTime game, allObjs game, hitScores game)
        f ho = let (x, y) = toFloatPair $ position ho in curTime >= getSec ho + 0.05 && getSec ho + 0.25 > curTime

handleInput :: Event -> BOsu -> BOsu
handleInput (EventMotion pos) game = game {cursorLoc = pos}
handleInput (EventKey (Char c) _ _ pos) game = if c == 'z' || c == 'x'
    then game {hitScores = checkHit pos game}
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
    sliderball = makeSliderball game'
    in game' {otherCircs = approachCircs ++ sliderball}

main :: IO ()
main = do
    mapPath <- head . filter (".osu" `isSuffixOf`) <$> listFiles "beatmap/"
    file <- T.readFile mapPath
    let (audio, curBMap) = parseFile mapPath file
    SDL.initialize [SDL.InitAudio]
    _ <- Mixer.openAudio Mixer.defaultAudio 4096
    audio <- Mixer.load $ "beatmap/" ++ T.unpack audio
    _ <- Mixer.play audio
    file <- T.readFile mapPath
    play windowDisplay black 120 curBMap render handleInput update
