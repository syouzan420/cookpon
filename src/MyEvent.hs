module MyEvent(inputEvent) where

import SDL (get,($=))
import SDL.Event (EventPayload(KeyboardEvent),eventPayload,keyboardEventKeyMotion
                 ,InputMotion(Pressed,Released),keyboardEventKeysym,EventWatchCallback)
import SDL.Input.Keyboard (Keysym(keysymKeycode))
import SDL.Input.Keyboard.Codes
import Data.IORef(IORef)
import qualified Data.Text as T
import MyData (State(..),initKeyEventCount)

inputEvent :: IORef State -> EventWatchCallback 
inputEvent state event = do
  st <- get state
  let kc = kec st
  let dr = dir st
  let tx = txt st
  let lc = lec st
  let ti = txi st
  let len = length tx
  let tlen = T.length (tx!!ti)
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
  let eventIsSpaceRelease e =
        case eventPayload e of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Released &&
              keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeSpace
          _ -> False
      hReleased = eventIsHRelease event
      jReleased = eventIsJRelease event
      kReleased = eventIsKRelease event
      lReleased = eventIsLRelease event
      spReleased = eventIsSpaceRelease event

      hPressed = eventIsHPress event
      jPressed = eventIsJPress event
      kPressed = eventIsKPress event
      lPressed = eventIsLPress event
  let ndr
        | kc==0 && (hPressed || hReleased) = 3
        | kc==0 && (lPressed || lReleased) = 7
        | kc==0 && (jPressed || jReleased) = 1
        | kc==0 && (kPressed || kReleased) = 9
        | otherwise = dr
  let nkec = if kc==0 && ndr/=0 then initKeyEventCount else kc
  let bNewLine = spReleased && (tlen==lc+1 && len-1>ti)
  let nti = if bNewLine then ti+1 else ti 
      nlec = if bNewLine then 0 else lc
      st' = st{kec=nkec,dir=ndr,lec=nlec,txi=nti}
  state $= st'

