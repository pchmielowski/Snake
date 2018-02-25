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

width = 64

height = 32

start = Vector 4 4

end = Vector ((x start) + width) ((y start) + height)

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
      , snake = [start]
      , velocity = Vector 0 1
      , generator = g
      , meal = randomMealPosition g
      }

randomMealPosition g = Vector rand rand
  where
    rand = toInteger $ (mod . fst . next) g 40

data Vector =
  Vector Integer
         Integer

x (Vector x _) = x

y (Vector _ y) = y

data State = State
  { delta :: Integer
  , before :: Integer
  , snake :: [Vector]
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
  frameColor <- newColorID ColorRed ColorBlack 1
  snakeColor <- newColorID ColorBlue ColorBlack 2
  updateScreen frameColor snakeColor
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
    -- TODO: check colision with walls
  where
    nextFrame now =
      if (hitsItself)
        then error "Loser" -- TODO: handle
        else if (eatsMeal)
               then (resetTimer now)
                    { snake = newHead : snake state
                    , meal = randomMealPosition nextGenerator
                    , generator = nextGenerator
                    }
               else (resetTimer now) {snake = init $ newHead : snake state}
    hitsItself = any (samePosition newHead) $ tail $ snake state
    eatsMeal = samePosition (head (snake state)) $ meal state
    samePosition (Vector ax ay) (Vector bx by) = ax == bx && ay == by
    nextGenerator = (snd . next) $ generator state
    resetTimer now = state {delta = 0, before = now}
    newHead = updatePosition (head (snake state)) (velocity state)
    updatePosition (Vector x y) (Vector dx dy) = Vector (x + dx) (y + dy)
    -- TODO: do not allow to reverse direction
    changeDirection now ToLeft = (updateTime now) {velocity = Vector (-1) 0}
    changeDirection now ToRight = (updateTime now) {velocity = Vector 1 0}
    changeDirection now ToDown = (updateTime now) {velocity = Vector 0 1}
    changeDirection now ToUp = (updateTime now) {velocity = Vector 0 (-1)}
    updateTime now =
      state {delta = delta state + (now - before state), before = now}
    -- updateScreen :: Curses ()
    updateScreen frameColor snakeColor = do
      updateWindow w $ do
        clear
        setColor frameColor
        drawFrame start end
        setColor snakeColor
        mapM drawSnakePart $ snake state
        draw glyphPlus $ meal state
      render
    drawSnakePart = draw glyphStipple
    draw glyph (Vector x y) = do
      moveCursor y $ 2 * x
      drawGlyph glyph
      moveCursor y $ 2 * x + 1
      drawGlyph glyph
    drawFrame (Vector sx sy) (Vector ex ey) = do
      mapM_ drawBlock $ zip [sx - 1 .. ex + 1] $ repeat (sy - 1)
      mapM_ drawBlock $ zip [sx - 1 .. ex + 1] $ repeat (ey + 1)
      mapM_ drawBlock $ zip (repeat (sx - 1)) [sy - 1 .. ey + 1]
      mapM_ drawBlock $ zip (repeat (ex + 1)) [sy - 1 .. ey + 1]
      where
        drawBlock :: (Integer, Integer) -> Update ()
        drawBlock (x, y) = do
          moveCursor y x
          drawGlyph glyphStipple
