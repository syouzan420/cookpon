module MyEvent(inputEvent) where

import Data.IORef(IORef,readIORef,writeIORef)
import SDL.Event (EventPayload(KeyboardEvent),eventPayload,keyboardEventKeyMotion
                 ,InputMotion(Pressed),keyboardEventKeysym,EventWatchCallback)
import SDL.Input.Keyboard (Keysym(keysymKeycode))
import SDL.Input.Keyboard.Codes
import MyData (State(..),initKeyEventCount)

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
{--
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
      hReleased = eventIsHRelease event
      jReleased = eventIsJRelease event
      kReleased = eventIsKRelease event
      lReleased = eventIsLRelease event
--}
      hPressed = eventIsHPress event
      jPressed = eventIsJPress event
      kPressed = eventIsKPress event
      lPressed = eventIsLPress event
  let ndr
--        | hReleased || jReleased || kReleased || lReleased = 0
        | kc==0 && hPressed = 3
        | kc==0 && lPressed = 7
        | kc==0 && jPressed = 1
        | kc==0 && kPressed = 9
        | otherwise = dr
  let nkec = if kc==0 then initKeyEventCount else kc
      st' = st{kec=nkec,dir=ndr}
  writeIORef state st'

