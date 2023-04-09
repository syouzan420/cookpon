{-# LANGUAGE OverloadedStrings #-}
module MyApp(appMain) where

import SDL.Init (initializeAll)
import SDL.Image (load)
import qualified SDL.Mixer as M
import SDL.Time (addTimer,removeTimer)
import qualified SDL.Font as F
import SDL.Video (createWindow,defaultWindow,windowInitialSize,destroyWindow
                 ,createRenderer,defaultRenderer)
import SDL.Event (addEventWatch,delEventWatch)
import SDL.Video.Renderer (createTextureFromSurface,present,freeSurface)
import SDL.Vect (V2(..),V4(..))
import Data.IORef(newIORef)
import MyData (fontSize,hiragana,initState)
import MyTimer (mainTimer)
import MyEvent (inputEvent)
import MyDraw (initDraw)
import MyLoop (appLoop)

appMain :: IO ()
appMain = do
  initializeAll
  let myWindow = defaultWindow {windowInitialSize =  V2 480 600}
  F.initialize
  font1 <- F.load "font/monaco.ttf" fontSize 
  font2 <- F.load "font/marugo.TTC" fontSize 
  font3 <- F.load "font/oshide.otf" fontSize 
  fontS1 <- F.blended font1 (V4 255 255 255 255) "abcdefghijklmnopqrstuvwxyz" 
  fontS2 <- F.blended font2 (V4 255 255 255 255) hiragana
  fontS3 <- F.blended font3 (V4 255 255 255 255) hiragana
  mapM_ F.free [font1,font2,font3]
  image <- load "images/cook5.png"
  image2 <- load "images/cook6.png"
  window <- createWindow "COOKPON" myWindow
  renderer <- createRenderer window (-1) defaultRenderer
  ftexs <- mapM (createTextureFromSurface renderer) [fontS1,fontS2,fontS3]
  itexs <- mapM (createTextureFromSurface renderer) [image,image2]
  mapM_ freeSurface [fontS1,fontS2,fontS3,image,image2]
  state <- newIORef initState
  initDraw renderer
  present renderer
  tm <- addTimer 30 (mainTimer state renderer ftexs itexs)
  ev <- addEventWatch (inputEvent state)
  M.withAudio M.defaultAudio 1024 $ do
    M.load "music/test.mp3" >>= M.playMusic M.Forever
    appLoop
  delEventWatch ev
  _ <- removeTimer tm
  destroyWindow window

