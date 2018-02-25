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
    loop w 0 t 0

data State = State
  { time :: Integer
  }

loop :: Window -> Integer -> Integer -> Integer -> Curses ()
loop w delta before x = do
  now <- liftIO $ currentTime
  updateScreen
  if (delta > frameDelay)
    then loop w 0 now $ x + 1
    else do
      ev <- getEvent w $ Just 0
      case ev of
        Nothing -> loop w (delta + (now - before)) now x
        Just ev' ->
          if (ev' == EventCharacter 'q')
            then return ()
            else loop w (delta + (now - before)) now x
  where
    updateScreen :: Curses ()
    updateScreen = do
      updateWindow w $ do
        clear
        moveCursor 3 x
        drawGlyph glyphStipple
      render
