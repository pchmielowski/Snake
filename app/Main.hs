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
      State
      { delta = 0
      , before = t
      , pos = Vector {x = 0, y = 0}
      , velocity = Vector {x = 0, y = 0}
      , generator = g
      , meal = randomMealPosition g
      }

randomMealPosition g = Vector {x = rand, y = rand}
  where
    rand = toInteger $ (mod . fst . next) g 40

data Vector = Vector
  { x :: Integer
  , y :: Integer
  }

data State = State
  { delta :: Integer
  , before :: Integer
  , pos :: Vector
  , velocity :: Vector
  , generator :: StdGen
  , meal :: Vector
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
      if (x (pos state) == x (meal state) && y (pos state) == y (meal state))
        then (nexPosition now)
             { meal = randomMealPosition nextGenerator
             , generator = nextGenerator
             }
        else nexPosition now
    nextGenerator = (snd . next) (generator state)
    nexPosition now =
      state
      { delta = 0
      , before = now
      , pos =
          Vector
          { x = x (pos state) + x (velocity state)
          , y = y (pos state) + y (velocity state)
          }
      }
    changeDirection now ToLeft =
      (updateTime now) {velocity = Vector {x = (-1), y = 0}}
    changeDirection now ToRight =
      (updateTime now) {velocity = Vector {x = 1, y = 0}}
    changeDirection now ToDown =
      (updateTime now) {velocity = Vector {x = 0, y = 1}}
    changeDirection now ToUp =
      (updateTime now) {velocity = Vector {x = 0, y = (-1)}}
    updateTime now =
      state {delta = delta state + (now - before state), before = now}
    updateScreen :: Curses ()
    updateScreen = do
      updateWindow w $ do
        clear
        moveCursor (y (pos state)) (x (pos state))
        drawGlyph glyphStipple
        moveCursor (y (meal state)) (x (meal state))
        drawGlyph glyphPlus
      render
