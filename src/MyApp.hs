module MyApp(appMain) where

import Data.IORef(newIORef)
import MySDL.MyLoad (myLoad)
import MySDL.MyLoop (myLoop)
import MySDL.MyInit (myInit)
import MySDL.MyInitVideo (myInitVideo)
import MySDL.MyAudio (myAudio)
import MySDL.MyQuit (myQuit)

appMain :: IO ()
appMain = do
  myInit
  (newState,fontS,imageS) <- myLoad
  (window,renderer,ftexs,itexs) <- myInitVideo (fontS,imageS)
  state <- newIORef newState 
  myAudio
  myLoop state renderer ftexs itexs
  myQuit window 

