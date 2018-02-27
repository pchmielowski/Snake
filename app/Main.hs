module Main where

import Control.Monad.IO.Class
import Data.Time.Clock.POSIX
import System.Random
import UI.NCurses

import Lib

currentTime :: IO Integer
currentTime = fmap round $ fmap (* 1000) getPOSIXTime

width :: Integer
width = 40

height :: Integer
height = 20

start = Vector 4 4

end = Vector ((x start) + width) ((y start) + height)

main :: IO ()
main =
  runCurses $ do
    setEcho False
    setCursorMode CursorInvisible
    w <- defaultWindow
    t <- liftIO $ currentTime
    g <- liftIO $ getStdGen
    loop w $ initial t g

initial t g =
  InGame
  { delta = 0
  , before = t
  , snake = [start]
  , velocity = Vector 0 0
  , direction = ToRight
  , generator = g
  , meal = randomMealPosition g
  , points = 0
  , delay = 100
  }

randomMealPosition g = Vector (position x) (position y)
  where
    position f = rand (f start) (f end)
    rand from to = (mod . toInteger . fst . next) g (to - from) + from

type Time = Integer

type Position = Integer

data Vector =
  Vector Position
         Position
  deriving (Eq)

x (Vector x _) = x

y (Vector _ y) = y

data State
  = InGame { delta :: Time
           , before :: Time
           , snake :: [Vector]
           , velocity :: Vector
           , direction :: Direction
           , generator :: StdGen
           , meal :: Vector
           , points :: Int
           , delay :: Time }
  | Lost

data Direction
  = ToLeft
  | ToRight
  | ToUp
  | ToDown

loop :: Window -> State -> Curses ()
loop w Lost = do
  first <- newColorID ColorRed ColorBlack 5
  second <- newColorID ColorGreen ColorBlack 6
  updateWindow w $ do
    setColor first
    moveCursor 16 16
    drawString "you've lost xD"
    setColor second
    moveCursor 24 24
    drawString "Press any key.."
  render
  ev <- getEvent w Nothing
  case ev of
    Just ev' -> return ()
loop w state = do
  now <- liftIO $ currentTime
  frameColor <- newColorID ColorRed ColorBlack 1
  snakeColor <- newColorID ColorBlue ColorBlack 2
  updateScreen frameColor snakeColor
  if (delta state > delay state)
    then loop w $ nextFrame now
    else do
      ev <- getEvent w $ Just 0
      case ev of
        Nothing -> loop w $ updateTime now
        Just (EventSpecialKey KeyLeftArrow) ->
          loop w $ (updateTime now) {direction = ToLeft}
        Just (EventSpecialKey KeyRightArrow) ->
          loop w $ (updateTime now) {direction = ToRight}
        Just (EventSpecialKey KeyDownArrow) ->
          loop w $ (updateTime now) {direction = ToDown}
        Just (EventSpecialKey KeyUpArrow) ->
          loop w $ (updateTime now) {direction = ToUp}
        Just ev' ->
          if (ev' == EventCharacter 'q')
            then return ()
            else loop w $ updateTime now
  where
    nextFrame :: Time -> State
    nextFrame now =
      if (hitsWall || hitsItself)
        then Lost
        else let updated = resetTimer now
             in if (eatsMeal)
                  then updated
                       { snake = newHead : snake state
                       , meal = randomMealPosition nextGenerator
                       , generator = nextGenerator
                       , points = points state + 1
                       , delay = delay state - 3
                       }
                  else updated {snake = move $ snake state}
    updateVelocity :: Direction -> Vector
    updateVelocity ToLeft = turn (Vector (-1) 0) $ velocity state
    updateVelocity ToRight = turn (Vector (1) 0) $ velocity state
    updateVelocity ToUp = turn (Vector 0 (-1)) $ velocity state
    updateVelocity ToDown = turn (Vector 0 (1)) $ velocity state
    turn :: Vector -> Vector -> Vector
    turn (Vector 0 dy) (Vector x y) =
      if (y /= 0)
        then Vector x y
        else Vector 0 dy
    turn (Vector dx 0) (Vector x y) =
      if (x /= 0)
        then Vector x y
        else Vector dx 0
    move snake = init $ newHead : snake
    hitsWall =
      let x' = x newHead
          y' = y newHead
      in x' > x end || x' < x start || y' > y end || y' < y start
    hitsItself = any (newHead ==) $ tail $ snake state
    eatsMeal = head (snake state) == (meal state)
    nextGenerator = (snd . next) $ generator state
    resetTimer now = state {delta = 0, before = now}
    newHead =
      updatePosition (head (snake state)) $ updateVelocity $ direction state
    updatePosition (Vector x y) (Vector dx dy) = Vector (x + dx) (y + dy)
    updateTime now =
      state {delta = delta state + (now - before state), before = now}
    updateScreen frameColor snakeColor = do
      updateWindow w $ do
        clear
        setColor frameColor
        moveCursor (y start - 2) (scale (x start))
        drawString $ "Score: " ++ (show $ points state)
        drawFrame start end
        setColor snakeColor
        draw glyphStipple $ meal state
        mapM drawSnakePart $ snake state
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
        drawBlock :: (Position, Position) -> Update ()
        drawBlock (x, y) = do
          moveCursor y $ scale x
          drawGlyph glyphStipple
          moveCursor y $ scale x + 1
          drawGlyph glyphStipple
    scale x = 2 * x
