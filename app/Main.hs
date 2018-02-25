module Main where

import UI.NCurses

import Lib

main :: IO ()
main =
  runCurses $ do
    setEcho False
    w <- defaultWindow
    runGame w

runGame :: Window -> Curses ()
runGame w = loop
  where
    loop :: Curses ()
    loop = do
      updateScreen
      ev <- getEvent w Nothing
      case ev of
        Nothing -> loop
                -- Just (EventSpecialKey KeyRightArrow) -> loop frame $ move right state
                -- Just (EventSpecialKey KeyLeftArrow) -> loop frame $ move left state
                -- Just (EventSpecialKey KeyDownArrow) -> loop frame state
                -- Just (EventSpecialKey KeyUpArrow) -> loop frame state
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
