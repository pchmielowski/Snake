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
    loop w State {delta = 0, before = t, x = 0, dx = 0}

data State = State
  { delta :: Integer
  , before :: Integer
  , x :: Integer
  , dx :: Integer
  }

loop :: Window -> State -> Curses ()
loop w state = do
  now <- liftIO $ currentTime
  updateScreen
  if (delta state > frameDelay)
    then loop w $ nextFrame now
    else do
      ev <- getEvent w $ Just 0
      case ev of
        Nothing -> loop w $ updateTime now
        Just (EventSpecialKey KeyLeftArrow) -> loop w $ changeDirection now 1
        Just (EventSpecialKey KeyRightArrow) ->
          loop w $ changeDirection now (-1)
        Just ev' ->
          if (ev' == EventCharacter 'q')
            then return ()
            else loop w $ updateTime now
  where
    nextFrame now = state {delta = 0, before = now, x = x state + dx state}
    changeDirection now dx = (updateTime now) {dx = dx}
    updateTime now =
      state {delta = delta state + (now - before state), before = now}
    updateScreen :: Curses ()
    updateScreen = do
      updateWindow w $ do
        clear
        moveCursor 3 $ x state
        drawGlyph glyphStipple
      render
