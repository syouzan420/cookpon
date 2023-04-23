module MyApp(appMain) where

import Data.IORef(newIORef)
import MySDL.MyLoad (myLoad)
import MySDL.MyLoop (myLoop)
import MySDL.MyInit (withMyInit)
import MySDL.MyInitVideo (withMyVideo)
--import MySDL.MyAudio (withMyAudio)
import MyALUT.MyAudio (withMyAudio)
import MyData (initState)

appMain :: IO ()
appMain = 
  withMyInit $ do
    (docl,sur) <- myLoad 0
    withMyVideo sur $ 
      \(renderer,ftexs,itexs) -> do
        state <- newIORef initState 
        withMyAudio $ myLoop state docl renderer ftexs itexs

