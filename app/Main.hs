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

width :: Integer
width = 64

height :: Integer
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

-- TODO: generates meal outside frame
randomMealPosition g = Vector (position x) (position y)
  where
    position f = rand (f start) (f end)
    rand from to = (mod . toInteger . fst . next) g (from + to) - from

type Time = Integer

type Position = Integer

data Vector =
  Vector Position
         Position

x (Vector x _) = x

y (Vector _ y) = y

-- TODO: State = InGame | Lost
--        gdzie InGame to rekord, który teraz się nazywa State
data State = State
  { delta :: Time
  , before :: Time
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
  where
    nextFrame now =
      if (hitsWall) -- TODO: refactor these ifs
        then error "Loser" -- TODO: handle
        else if (hitsItself)
               then error "Loser" -- TODO: handle
               else if (eatsMeal)
                      then (resetTimer now)
                           { snake = newHead : snake state
                           , meal = randomMealPosition nextGenerator
                           , generator = nextGenerator
                           }
                      else (resetTimer now)
                           {snake = init $ newHead : snake state}
    hitsWall -- TODO: does not work correctly
     =
      let x' = x newHead
          y' = y newHead
      in x' > x end || x' < x start || y' > y end || y' < y start
    hitsItself = any (samePosition newHead) $ tail $ snake state
    eatsMeal = samePosition (head (snake state)) $ meal state
    samePosition (Vector ax ay) (Vector bx by) = ax == bx && ay == by
    nextGenerator = (snd . next) $ generator state
    resetTimer now = state {delta = 0, before = now}
    newHead = updatePosition (head (snake state)) (velocity state)
    updatePosition (Vector x y) (Vector dx dy) = Vector (x + dx) (y + dy)
    -- TODO: do not allow to reverse direction
    changeDirection now ToLeft = turnX (-1) now
    changeDirection now ToRight = turnX 1 now
    changeDirection now ToDown = turnY 1 now
    changeDirection now ToUp = turnY (-1) now
    turnX = turn x (\dir -> Vector dir 0)
    turnY = turn y (\dir -> Vector 0 dir)
    turn ::
         (Vector -> Position)
      -> (Position -> Vector)
      -> Position
      -> Time
      -> State
    turn get toVector direction now =
      let updated = updateTime now
      in if (get (velocity state) /= 0)
           then updated
           else updated {velocity = (toVector direction)}
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
      moveCursor y $ scale x
      drawGlyph glyph
      moveCursor y $ scale x + 1
      drawGlyph glyph
    drawFrame (Vector sx sy) (Vector ex ey) = do
      mapM_ drawBlock $ zip [sx - 1 .. ex + 1] $ repeat (sy - 1)
      mapM_ drawBlock $ zip [sx - 1 .. ex + 1] $ repeat (ey + 1)
      mapM_ drawBlock $ zip (repeat (sx - 1)) [sy - 1 .. ey + 1]
      mapM_ drawBlock $ zip (repeat (ex + 1)) [sy - 1 .. ey + 1]
      where
        drawBlock :: (Integer, Integer) -> Update ()
        drawBlock (x, y) = do
          moveCursor y $ scale x
          drawGlyph glyphStipple
          moveCursor y $ scale x + 1
          drawGlyph glyphStipple
    scale x = 2 * x
