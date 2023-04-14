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
  I.initialize []
  fontS <- loadFonts fontSize ["font/monaco.ttf","font/marugo.TTC","font/oshide.otf"]
  (newState,fontTS) <- loadText fontSize "font/marugo.TTC" "texts/test.txt" initState 
  imageS <- loadImages ["images/cook5.png","images/cook6.png"]
  let myWindow = defaultWindow {windowInitialSize =  V2 480 600}
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
  M.withAudio M.defaultAudio 1024 $ do
    M.load "music/cooktest1.mp3" >>= M.playMusic M.Forever
    appLoop
  delEventWatch ev
  _ <- removeTimer tm
  destroyWindow window

