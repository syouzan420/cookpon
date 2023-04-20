module MyApp(appMain) where

import Data.IORef(newIORef)
import MyLoad (myLoad)
import MyLoop (myLoop)
import MyInit (myInit)
import MyInitVideo (myInitVideo)
import MyAudio (myAudio)
import MyQuit (myQuit)

appMain :: IO ()
appMain = do
  myInit
  (newState,fontS,imageS) <- myLoad
  (window,renderer,ftexs,itexs) <- myInitVideo (fontS,imageS)
  state <- newIORef newState 
  myAudio
  myLoop state renderer ftexs itexs
  myQuit window 

