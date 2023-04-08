module MyEvent(inputEvent) where

import Data.IORef(IORef,readIORef,writeIORef)
import SDL.Event (EventPayload(KeyboardEvent),eventPayload,keyboardEventKeyMotion
                 ,InputMotion(Pressed,Released),keyboardEventKeysym,EventWatchCallback)
import SDL.Input.Keyboard (Keysym(keysymKeycode))
import SDL.Input.Keyboard.Codes
import MyData (State(..))

inputEvent :: IORef State -> EventWatchCallback 
inputEvent state event = do
  st <- readIORef state
  let kc = kec st
  let dr = dir st
  let eventIsHPress e =
        case eventPayload e of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
              keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeH
          _ -> False
  let eventIsJPress e =
        case eventPayload e of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
              keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeJ
          _ -> False
  let eventIsKPress e =
        case eventPayload e of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
              keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeK
          _ -> False
  let eventIsLPress e =
        case eventPayload e of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
              keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeL
          _ -> False
  let eventIsHRelease e =
        case eventPayload e of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Released &&
              keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeH
          _ -> False
  let eventIsJRelease e =
        case eventPayload e of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Released &&
              keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeJ
          _ -> False
  let eventIsKRelease e =
        case eventPayload e of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Released &&
              keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeK
          _ -> False
  let eventIsLRelease e =
        case eventPayload e of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Released &&
              keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeL
          _ -> False
      hPressed = eventIsHPress event
      jPressed = eventIsJPress event
      kPressed = eventIsKPress event
      lPressed = eventIsLPress event
      hReleased = eventIsHRelease event
      jReleased = eventIsJRelease event
      kReleased = eventIsKRelease event
      lReleased = eventIsLRelease event
  let ndr
        | hReleased || jReleased || kReleased || lReleased = 0
        | hPressed = 3
        | lPressed = 7
        | jPressed = 1
        | kPressed = 9
        | otherwise = dr
      st' = st{dir=ndr}
  writeIORef state st'

