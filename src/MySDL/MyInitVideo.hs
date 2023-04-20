module MySDL.MyInitVideo(myInitVideo) where

import SDL.Video (Window,createWindow,defaultWindow,windowInitialSize
                 ,createRenderer,defaultRenderer)
import SDL.Video.Renderer (Surface,Renderer,Texture,createTextureFromSurface,present,freeSurface)
import MySDL.MyDraw (initDraw)
import MyData (windowSize,title)

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