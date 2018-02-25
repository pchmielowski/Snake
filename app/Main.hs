module Main where

import UI.NCurses
import Data.Time.Clock.POSIX

import Lib

main :: IO ()
main =
  runCurses $ do
    setEcho False
    w <- defaultWindow
    runGame w

runGame w = loop
  where
    loop = do
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
