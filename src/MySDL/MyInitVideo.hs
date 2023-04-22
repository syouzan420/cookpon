module MySDL.MyInitVideo(myInitVideo,withMyVideo) where

import SDL.Video (Window,createWindow,defaultWindow,windowInitialSize
                 ,createRenderer,defaultRenderer,destroyWindow)
import SDL.Video.Renderer (Surface,Renderer,Texture,createTextureFromSurface,present,freeSurface)
import MySDL.MyDraw (initDraw)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import MyData (windowSize,title)

withMyVideo :: (MonadIO m) => ([Surface],[Surface]) -> ((Renderer,[Texture],[Texture]) -> m a) -> m ()
withMyVideo (fontS,imageS) op = do
  let myWindow = defaultWindow {windowInitialSize = windowSize}
  window <- createWindow title myWindow
  renderer <- createRenderer window (-1) defaultRenderer
  ftexs <- mapM (createTextureFromSurface renderer) fontS
  itexs <- mapM (createTextureFromSurface renderer) imageS
  mapM_ freeSurface (imageS++fontS)
  initDraw renderer
  present renderer
  void $ op (renderer,ftexs,itexs)
  destroyWindow window

myInitVideo :: ([Surface],[Surface]) -> IO (Window,Renderer,[Texture],[Texture]) 
myInitVideo (fontS,imageS) = do
  let myWindow = defaultWindow {windowInitialSize = windowSize}
  window <- createWindow title myWindow
  renderer <- createRenderer window (-1) defaultRenderer
  ftexs <- mapM (createTextureFromSurface renderer) fontS
  itexs <- mapM (createTextureFromSurface renderer) imageS
  mapM_ freeSurface (imageS++fontS)
  initDraw renderer
  present renderer
  return (window,renderer,ftexs,itexs)
