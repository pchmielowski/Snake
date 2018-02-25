module Main where

import Control.Monad.IO.Class
import Data.Time.Clock.POSIX
import UI.NCurses

import Lib

-- milliseconds
frameDelay = 123

currentTime = fmap round $ fmap (* 1000) getPOSIXTime

main :: IO ()
main = do
  t <- currentTime
  loop 0 t

loop delta before = do
  now <- currentTime
  if (delta > frameDelay)
    then do
      putStrLn $ show delta
      loop 0 now
    else loop (delta + (now - before)) now

-- main :: IO ()
-- main =
--   runCurses $ do
--     setEcho False
--     w <- defaultWindow
--     t <- liftIO $ currentTime
--     runGame w
data State = State
  { time :: Integer
  }

runGame w = loop
  where
    loop = do
      now <- liftIO $ round `fmap` getPOSIXTime
      updateScreen
      ev <- getEvent w Nothing
      case ev of
        Nothing -> loop
        Just ev' ->
          if (ev' == EventCharacter 'q')
            then return ()
            else loop
    updateScreen :: Curses ()
    updateScreen = do
      updateWindow w $ do
        clear
        moveCursor 3 3
        drawGlyph glyphStipple
      render
