module MySDL.MyInput(myInput) where

import SDL.Event (EventPayload(KeyboardEvent),eventPayload,keyboardEventKeyMotion
                 ,InputMotion(Pressed,Released),keyboardEventKeysym,pollEvents)
import SDL.Input.Keyboard (Keysym(keysymKeycode))
import SDL.Input.Keyboard.Codes


myInput :: IO [Bool]
myInput = do
  events <- pollEvents
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

  return [qPressed,spReleased,hPressed,jPressed,kPressed,lPressed]
