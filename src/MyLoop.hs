module MyLoop(appLoop) where

import Control.Monad (unless)
import SDL.Event (EventPayload(KeyboardEvent),pollEvents,eventPayload
                 ,keyboardEventKeyMotion,InputMotion(Pressed),keyboardEventKeysym)
import SDL.Input.Keyboard (Keysym(keysymKeycode))
import SDL.Input.Keyboard.Codes
import SDL.Time(delay)

appLoop :: IO ()
appLoop = do 
  events <- pollEvents
  let eventIsQPress e =
        case eventPayload e of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
  delay 50 
  unless qPressed appLoop

