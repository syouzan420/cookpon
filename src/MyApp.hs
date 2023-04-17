module MyApp(appMain) where

import SDL.Init (initializeAll)
import qualified SDL.Mixer as M
import qualified SDL.Font as F
import qualified SDL.Image as I
import SDL.Video (createWindow,defaultWindow,windowInitialSize,destroyWindow
                 ,createRenderer,defaultRenderer)
import SDL.Video.Renderer (createTextureFromSurface,present,freeSurface)
import Data.IORef(newIORef)
import MyData (Fchr(..),fontSize,initState,fontFiles,imageFiles,textFiles,musicFiles
              ,title,windowSize)
import MyDraw (initDraw)
import MyLoad (loadImages,loadText)
import MyLoop (appLoop)

appMain :: IO ()
appMain = do
  initializeAll
  F.initialize
  I.initialize []
  (newState,fontTS) <- loadText fontSize Hi fontFiles (head textFiles) initState 
  imageS <- loadImages imageFiles 
  let myWindow = defaultWindow {windowInitialSize = windowSize}
  window <- createWindow title myWindow
  renderer <- createRenderer window (-1) defaultRenderer
  ftexs <- mapM (createTextureFromSurface renderer) fontTS
  itexs <- mapM (createTextureFromSurface renderer) imageS
  mapM_ freeSurface (imageS++fontTS)
  initDraw renderer
  present renderer
  state <- newIORef newState 
  M.withAudio M.defaultAudio 256 $ do
    M.load (head musicFiles) >>= M.playMusic M.Forever
    cd <- M.chunkDecoders
    md <- M.musicDecoders
    print cd
    print md
    appLoop state renderer ftexs itexs
  destroyWindow window

myAudio :: M.Audio
myAudio = M.Audio 11025 M.FormatS16_Sys M.Stereo
