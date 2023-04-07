module MyEvent(inputEvent) where

import Data.IORef(IORef,readIORef,writeIORef)
import SDL.Event (EventPayload(KeyboardEvent),eventPayload,keyboardEventKeyMotion
                 ,InputMotion(Pressed),keyboardEventKeysym,EventWatchCallback)
import SDL.Input.Keyboard (Keysym(keysymKeycode))
import SDL.Input.Keyboard.Codes
import SDL.Vect (V2(..))
import MyData (State(..))

inputEvent :: IORef State -> EventWatchCallback
inputEvent state event = do
  st <- readIORef state
  let kc = kec st
  let (V2 px py) = pos st
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
      hPressed = (kc==0) && eventIsHPress event
      jPressed = (kc==0) && eventIsJPress event
      kPressed = (kc==0) && eventIsKPress event
      lPressed = (kc==0) && eventIsLPress event
  let dp = 16 
  let npx 
        | hPressed = px-dp
        | lPressed = px+dp
        | otherwise = px
  let npy
        | jPressed = py+dp
        | kPressed = py-dp
        | otherwise = py
      st' = st{pos=V2 npx npy}
  writeIORef state st'

