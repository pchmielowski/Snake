module Main where

import Control.Monad.IO.Class
import Data.Time.Clock.POSIX
import UI.NCurses

import Lib

-- milliseconds
frameDelay = 123

currentTime :: IO Integer
currentTime = fmap round $ fmap (* 1000) getPOSIXTime

-- main :: IO ()
-- main = do
--   t <- currentTime
--   loop 0 t
-- loop delta before = do
--   now <- currentTime
--   if (delta > frameDelay)
--     then do
--       putStrLn $ show delta
--       loop 0 now
--     else loop (delta + (now - before)) now
main :: IO ()
main =
  runCurses $ do
    setEcho False
    w <- defaultWindow
    t <- liftIO $ currentTime
    loop w 0 t 0 0

data State = State
  { time :: Integer
  }

loop :: Window -> Integer -> Integer -> Integer -> Integer -> Curses ()
loop w delta before x dx = do
  now <- liftIO $ currentTime
  updateScreen
  if (delta > frameDelay)
    then loop w 0 now (x + dx) dx
    else do
      ev <- getEvent w $ Just 0
      case ev of
        Nothing -> loop w (delta + (now - before)) now x dx
        Just (EventSpecialKey KeyLeftArrow) ->
          loop w (delta + (now - before)) now x (-1)
        Just (EventSpecialKey KeyRightArrow) ->
          loop w (delta + (now - before)) now x 1
        Just (EventSpecialKey KeyUpArrow) ->
          loop w (delta + (now - before)) now x 0
        Just (EventSpecialKey KeyDownArrow) ->
          loop w (delta + (now - before)) now x 0
        Just ev' ->
          if (ev' == EventCharacter 'q')
            then return ()
            else loop w (delta + (now - before)) now x dx
  where
    updateScreen :: Curses ()
    updateScreen = do
      updateWindow w $ do
        clear
        moveCursor 3 x
        drawGlyph glyphStipple
      render
