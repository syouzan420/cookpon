module MyLoop(appLoop) where

import Control.Monad (unless)
import Data.IORef(IORef)
import SDL(get,($=))
import SDL.Video.Renderer (Renderer,Texture)
import SDL.Time(delay)
import MyAction(mainAction)
import MyData(State(..))
import MyEvent(inputEvent)

appLoop :: IORef State -> Renderer -> [Texture] -> [Texture] -> IO ()
appLoop state re ftexs itexs = do 
  st <- get state
  let time = tim st
  (nst,qPressed) <- inputEvent st
  nst' <- mainAction nst re ftexs itexs
  state $= nst'{tim=time+1}
  print time
  delay 50 
  unless qPressed (appLoop state re ftexs itexs)

