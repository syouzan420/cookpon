module MyEvent(inputEvent) where

import SDL.Event (EventPayload(KeyboardEvent),eventPayload,keyboardEventKeyMotion
                 ,InputMotion(Pressed,Released),keyboardEventKeysym,pollEvents)
import SDL.Input.Keyboard (Keysym(keysymKeycode))
import SDL.Input.Keyboard.Codes
import SDL.Vect (V2(..))
import qualified Data.Text as T
import MyData (State(..),initKeyEventCount,initGamePosition,charaSize)

inputEvent :: State -> IO (State,Bool)
inputEvent st = do
  events <- pollEvents
  let (V2 px py) = pos st
  let mp = gmp st
  let kc = kec st
  let dr = dir st
  let tx = txt st
  let lc = lec st
  let ti = txi st
  let len = length tx
  let tlen = T.length (tx!!ti)
  let eventIsQPress e =
        case eventPayload e of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
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
--}
  let eventIsSpaceRelease e =
        case eventPayload e of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Released &&
              keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeSpace
          _ -> False
{--
      hReleased = any eventIsHRelease events
      jReleased = any eventIsJRelease events
      kReleased = any eventIsKRelease events
      lReleased = any eventIsLRelease events
--}
      qPressed = any eventIsQPress events
      spReleased = any eventIsSpaceRelease events
      hPressed = any eventIsHPress events
      jPressed = any eventIsJPress events
      kPressed = any eventIsKPress events
      lPressed = any eventIsLPress events
  let (V2 lfp upp) = initGamePosition
  let iLeftEdge = lfp >= px 
  let iUpEdge = upp >= py
  let mapHLength = fromIntegral$length (head mp)
  let mapVLength = fromIntegral$length mp
  let iRightEdge = lfp+(mapHLength-1)*charaSize <= px
  let iDownEdge = upp+(mapVLength-1)*charaSize <= py
  let ndr
        | kc==0 && not iLeftEdge && hPressed = 3
        | kc==0 && not iRightEdge && lPressed = 7
        | kc==0 && not iDownEdge && jPressed = 1
        | kc==0 && not iUpEdge && kPressed = 9
        | kc==0 = 0
        | otherwise = dr
  let nkec = if kc==0 && ndr/=0 then initKeyEventCount else kc
  let bNewLine = spReleased && (tlen==lc+1 && len-1>ti)
  let nti = if bNewLine then ti+1 else ti 
      nlec = if bNewLine then 0 else lc
      st' = st{kec=nkec,dir=ndr,lec=nlec,txi=nti}
  return (st',qPressed)

