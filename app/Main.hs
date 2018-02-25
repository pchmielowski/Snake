module Main where

import Control.Monad.IO.Class
import Data.Time.Clock.POSIX
import System.Random
import UI.NCurses

import Lib

-- milliseconds
frameDelay = 123

currentTime :: IO Integer
currentTime = fmap round $ fmap (* 1000) getPOSIXTime

main :: IO ()
main =
  runCurses $ do
    setEcho False
    w <- defaultWindow
    t <- liftIO $ currentTime
    g <- liftIO $ getStdGen
    loop
      w
      State {delta = 0, before = t, x = 0, y = 0, dx = 0, dy = 0, generator = g}

data State = State
  { delta :: Integer
  , before :: Integer
  , x :: Integer
  , dx :: Integer
  , y :: Integer
  , dy :: Integer
  , generator :: StdGen
  }

data Direction
  = ToLeft
  | ToRight
  | ToUp
  | ToDown

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
        Just (EventSpecialKey KeyLeftArrow) ->
          loop w $ changeDirection now ToLeft
        Just (EventSpecialKey KeyRightArrow) ->
          loop w $ changeDirection now ToRight
        Just (EventSpecialKey KeyDownArrow) ->
          loop w $ changeDirection now ToDown
        Just (EventSpecialKey KeyUpArrow) -> loop w $ changeDirection now ToUp
        Just ev' ->
          if (ev' == EventCharacter 'q')
            then return ()
            else loop w $ updateTime now
  where
    nextFrame now =
      state
      {delta = 0, before = now, x = x state + dx state, y = y state + dy state}
    changeDirection now ToLeft = (updateTime now) {dx = (-1), dy = 0}
    changeDirection now ToRight = (updateTime now) {dx = 1, dy = 0}
    changeDirection now ToDown = (updateTime now) {dx = 0, dy = 1}
    changeDirection now ToUp = (updateTime now) {dx = 0, dy = (-1)}
    updateTime now =
      state {delta = delta state + (now - before state), before = now}
    updateScreen :: Curses ()
    updateScreen = do
      updateWindow w $ do
        clear
        moveCursor (y state) (x state)
        drawGlyph glyphStipple
        moveCursor
          (toInteger ((mod . fst . next) (generator state) 40))
          (toInteger ((mod . fst . next) (generator state) 40))
        drawGlyph glyphPlus
      render
