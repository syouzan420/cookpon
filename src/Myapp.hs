{-# LANGUAGE OverloadedStrings #-}
module Myapp where

import SDL.Init (initializeAll)
import SDL.Image (load)
import SDL.Time (TimerCallback,addTimer,RetriggerTimer(Reschedule))
import qualified SDL.Font as F
import SDL.Video (createWindow,defaultWindow,windowInitialSize,destroyWindow
                 ,createRenderer,defaultRenderer,Renderer,Texture)
import SDL.Event (EventPayload(KeyboardEvent),pollEvents,eventPayload,keyboardEventKeyMotion
                 ,InputMotion(Pressed),keyboardEventKeysym)
import SDL.Video.Renderer (loadBMP,createTextureFromSurface,rendererDrawColor,clear,drawPoint
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

type Pos = (Double,Double)
data State = State{pos:: V2 CInt}

initState = State{pos=V2 100 100}

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
  mapM freeSurface [image,image2]
  state <- newIORef initState
  tm <- addTimer 500 (moveChara state renderer texs)
  appLoop 
  destroyWindow window

appLoop :: IO ()
appLoop = do
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
  unless qPressed appLoop

moveChara :: IORef State -> Renderer -> [Texture] -> TimerCallback
moveChara state re te i = do
  st <- readIORef state
  let ps@(V2 px py) = pos st
  rendererDrawColor re $= V4 182 100 255 255
  clear re
  rendererDrawColor re $= V4 0 0 0 255
  drawPoint re (P (V2 100 100))
  drawLine re (P (V2 50 50)) (P (V2 80 80))
  let rects = map (\(V4 x y w h) -> Rectangle (P (V2 x y)) (V2 w h))
                                    [V4 30 300 300 50,V4 200 200 100 100,V4 px py 50 50]
  mapM_ (\(s,tex) -> copy re tex Nothing (Just s)) (zip rects te)
  showOneChar re (head te) 40 (V2 50 400) 'f'
  --copy re (te!!2) Nothing (Just$Rectangle (P ps) (V2 50 50))
  present re
  let npos = ps + (V2 3 2)
  let st' = st{pos=npos}
  writeIORef state st'
  return (Reschedule i)

showOneChar :: MonadIO m => Renderer -> Texture -> CInt -> V2 CInt -> Char -> m ()
showOneChar r t s p ch =
  let (Just index) = elemIndex ch ['a'..'z']
   in copy r t (Just (Rectangle (P (V2 (fromIntegral index*14) 0)) (V2 14 24)))
               (Just (Rectangle (P p) (V2 s s)))

