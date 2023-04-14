{-#LANGUAGE OverloadedStrings #-}
module MyApp(appMain) where

import SDL.Init (initializeAll)
import qualified SDL.Mixer as M
import SDL.Time (addTimer,removeTimer)
import qualified SDL.Font as F
import qualified SDL.Image as I
import SDL.Video (createWindow,defaultWindow,windowInitialSize,destroyWindow
                 ,createRenderer,defaultRenderer)
import SDL.Event (addEventWatch,delEventWatch)
import SDL.Video.Renderer (createTextureFromSurface,present,freeSurface)
import SDL.Vect (V2(..))
import Data.IORef(newIORef)
import MyData (fontSize,initState)
import MyTimer (mainTimer)
import MyEvent (inputEvent)
import MyDraw (initDraw)
import MyLoad (loadImages,loadFonts,loadText)
import MyLoop (appLoop)

appMain :: IO ()
appMain = do
  initializeAll
  F.initialize
<<<<<<< HEAD
  font1 <- F.load "font/monaco.ttf" fontSize 
  font2 <- F.load "font/marugo.TTC" fontSize 
  font3 <- F.load "font/oshide.otf" fontSize 
  fontS1 <- F.blended font1 (V4 255 255 255 255) "abcdefghijklmnopqrstuvwxyz" 
  fontS2 <- F.blended font2 (V4 255 255 255 255) hiragana
  fontS3 <- F.blended font3 (V4 255 255 255 255) hiragana
  mapM_ F.free [font1,font2,font3]
  image <- load "images/takapon1.png"
  image2 <- load "images/takapon2.png"
=======
  I.initialize []
  fontS <- loadFonts fontSize ["font/monaco.ttf","font/marugo.TTC","font/oshide.otf"]
  (newState,fontTS) <- loadText fontSize "font/marugo.TTC" "texts/test.txt" initState 
  imageS <- loadImages ["images/cook5.png","images/cook6.png"]
  let myWindow = defaultWindow {windowInitialSize =  V2 480 600}
>>>>>>> 60ab7945918ba7dec5c5f028409f3612bc09332e
  window <- createWindow "COOKPON" myWindow
  renderer <- createRenderer window (-1) defaultRenderer
  ftexs <- mapM (createTextureFromSurface renderer) fontS 
  ftex2 <- mapM (createTextureFromSurface renderer) fontTS
  itexs <- mapM (createTextureFromSurface renderer) imageS
  mapM_ freeSurface (imageS++fontTS)
  initDraw renderer
  present renderer
  state <- newIORef initState 
  tm <- addTimer 30 (mainTimer state renderer ftexs itexs)
  ev <- addEventWatch (inputEvent state)
<<<<<<< HEAD
  M.withAudio M.defaultAudio 4096 $ do
    M.load "music/test.mp3" >>= M.playMusic M.Forever
=======
  M.withAudio M.defaultAudio 1024 $ do
    M.load "music/cooktest2.mp3" >>= M.playMusic M.Forever
>>>>>>> 60ab7945918ba7dec5c5f028409f3612bc09332e
    appLoop
  delEventWatch ev
  _ <- removeTimer tm
  destroyWindow window

