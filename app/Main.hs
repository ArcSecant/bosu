{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

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

-- type FileResults = Maybe (Either (FormatVersion, ParseErrorBundle T.Text (ErrorItem T.Text)) G.Beatmap)
data VHO = Vector HitObject

hCircle :: (Int, Int) -> Float -> Picture
hCircle (x, y) r = Translate (fromIntegral x) (fromIntegral y)  (Color cyan $ circleSolid r)

aCircle :: (Int, Int) -> Float -> Picture
aCircle (x, y) r = Translate (fromIntegral x) (fromIntegral y)  (Color white $ Circle r)

audioPath = "C:\\Program Files (x86)\\osu!\\Songs\\1202012 Magus Two - BEAUTIFUL SENTENCE (TV Size)\\audio.mp3"
mapPath = "C:\\Program Files (x86)\\osu!\\Songs\\1202012 Magus Two - BEAUTIFUL SENTENCE (TV Size)\\Magus Two - BEAUTIFUL SENTENCE (TV Size) (Sephira) [Luxuria].osu"

parseFile :: FilePath -> T.Text -> [HitObject]
parseFile path content = case parse beatmap path content of
    Left  e -> []
    Right b -> hitObjects b

windowDisplay :: Display
windowDisplay = InWindow "BOsu" (1080, 720) (10, 10)

animationFunc :: Float -> [HitObject] -> Float -> Picture
animationFunc ar hitobjs t = Pictures ((map (\ho -> hCircle (position ho) 40) visibleObjs) ++ approachCircs)
    where
        visibleObjs = filter (\x -> (fromIntegral $ time x)/1000 <= t + ar && (fromIntegral $ time x)/1000 >= t) hitobjs 
        approachCircs = map (\x -> aCircle (position x) $ 40.0 * (1.0 + 2.0 * ((fromIntegral $ time x)/1000 - t) / ar)) visibleObjs
    
main :: IO ()
main = do
    SDL.initialize [SDL.InitAudio]
    res <- Mixer.openAudio Mixer.defaultAudio 4096
    play <- Mixer.load audioPath
    p <- Mixer.playForever play
    file <- T.readFile mapPath
    let curBMap = parseFile mapPath file in
        animate windowDisplay black $ animationFunc 0.5 curBMap
