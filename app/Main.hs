{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Game
import Graphics.Gloss
import qualified SDL
import qualified SDL.Mixer as Mixer
import System.Directory.Extra (listFiles)
import System.Environment (getArgs)

windowDisplay :: Display
windowDisplay = InWindow "BOsu" (1080, 720) (50, 50)

main :: IO ()
main = do
    args <- getArgs
    mapPath <-
        ( if null args
                then head . filter (".osu" `isSuffixOf`) <$> listFiles "beatmap/"
                else (!! read (head args)) . filter (".osu" `isSuffixOf`) <$> listFiles "beatmap/"
            )
    file <- T.readFile mapPath
    let (audio, curBMap) = parseFile mapPath file
    SDL.initialize [SDL.InitAudio]
    _ <- Mixer.openAudio Mixer.defaultAudio 4096
    audio <- Mixer.load $ "beatmap/" ++ T.unpack audio
    _ <- Mixer.play audio
    file <- T.readFile mapPath
    play windowDisplay black 120 curBMap render handleInput update
