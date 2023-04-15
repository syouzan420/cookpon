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
import Data.IORef(newIORef)
import MyData (fontSize,initState,fontFiles,imageFiles,textFiles,musicFiles,timerInterval,title,windowSize)
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
  fontS <- loadFonts fontSize fontFiles
  --(newState,fontTS) <- loadText fontSize (fontFiles!!1) (head textFiles) initState 
  imageS <- loadImages  imageFiles 
  let myWindow = defaultWindow {windowInitialSize = windowSize}
  window <- createWindow title myWindow
  renderer <- createRenderer window (-1) defaultRenderer
  ftexs <- mapM (createTextureFromSurface renderer) fontS 
  --ftex2 <- mapM (createTextureFromSurface renderer) fontTS
  itexs <- mapM (createTextureFromSurface renderer) imageS
  mapM_ freeSurface (imageS++fontS)
  initDraw renderer
  present renderer
  state <- newIORef initState 
  tm <- addTimer timerInterval (mainTimer state renderer ftexs itexs)
  ev <- addEventWatch (inputEvent state)
  M.withAudio M.defaultAudio 1024 $ do
    M.load (head musicFiles) >>= M.playMusic M.Forever
    appLoop
  delEventWatch ev
  _ <- removeTimer tm
  destroyWindow window

