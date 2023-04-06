{-# LANGUAGE OverloadedStrings #-}
module Myapp where

import SDL.Init (initializeAll)
import SDL.Image (load)
import SDL.Time (TimerCallback,addTimer,removeTimer,RetriggerTimer(Reschedule))
import qualified SDL.Font as F
import SDL.Video (createWindow,defaultWindow,windowInitialSize,destroyWindow
                 ,createRenderer,defaultRenderer,Renderer,Texture)
import SDL.Event (EventPayload(KeyboardEvent),pollEvents,eventPayload,keyboardEventKeyMotion
                 ,InputMotion(Pressed),keyboardEventKeysym,addEventWatch
                 ,delEventWatch,EventWatchCallback)
import SDL.Video.Renderer (createTextureFromSurface,rendererDrawColor,clear,drawPoint
                          ,drawLine,copy,present,Rectangle(..),freeSurface)
import Data.IORef(IORef,newIORef,readIORef,writeIORef)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import SDL.Vect (Point(P),V2(..),V4(..))
import SDL.Input.Keyboard (Keysym(keysymKeycode))
import SDL.Input.Keyboard.Codes
import SDL (($=))
import Foreign.C.Types (CInt)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

type Pos = (Double,Double)
data State = State{pos:: V2 CInt, kec:: CInt}

initState :: State
initState = State{pos=V2 100 100, kec=0} --kec:KeyboardEventCount

initKeyEventCount :: CInt
initKeyEventCount = 10

appMain :: IO ()
appMain = do
  initializeAll
  let myWindow = defaultWindow {windowInitialSize =  V2 480 600}
  F.initialize
  font <- F.load "font/monaco.ttf" 24
  fontS <- F.blended font (V4 255 255 255 255) "abcdefghijklmnopqrstuvwxyz" 
  F.free font
  window <- createWindow "COOKPON" myWindow
  image <- load "images/kagunomi.png"
  image2 <- load "images/chara.png"
  renderer <- createRenderer window (-1) defaultRenderer
  texs <- mapM (createTextureFromSurface renderer) [fontS,image,image2]
  mapM_ freeSurface [image,image2]
  state <- newIORef initState
  tm <- addTimer 50 (moveChara state renderer texs)
  ev <- addEventWatch (inputEvent state)
  appLoop
  delEventWatch ev
  _ <- removeTimer tm
  destroyWindow window

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
  unless qPressed appLoop

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


moveChara :: IORef State -> Renderer -> [Texture] -> TimerCallback
moveChara state re te i = do
  st <- readIORef state
  let ps = pos st
  let kc = kec st
  rendererDrawColor re $= V4 182 100 255 255
  clear re
  rendererDrawColor re $= V4 0 0 0 255
  drawPoint re (P (V2 100 100))
  drawLine re (P (V2 50 50)) (P (V2 80 80))
  let rects = map (\(V4 x y w h) -> Rectangle (P (V2 x y)) (V2 w h))
                                    [V4 30 300 300 50,V4 200 200 100 100,V4 400 400 50 50]
  mapM_ (\(s,tex) -> copy re tex Nothing (Just s)) (zip rects te)
  showOneChar re (head te) 40 (V2 50 400) 'f'
  let chara = if kc> (initKeyEventCount `div` 2) then te!!1 else te!!2
  copy re chara Nothing (Just$Rectangle (P ps) (V2 50 50))
  present re
  let nkec = if kc==0 then initKeyEventCount else kc-1
  let st' = st{kec=nkec}
  writeIORef state st'
  return (Reschedule i)

showOneChar :: MonadIO m => Renderer -> Texture -> CInt -> V2 CInt -> Char -> m ()
showOneChar r t s p ch =
  let index = fromMaybe (-1) (elemIndex ch ['a'..'z'])
   in copy r t (Just (Rectangle (P (V2 (fromIntegral index*14) 0)) (V2 14 24)))
               (Just (Rectangle (P p) (V2 s s)))

