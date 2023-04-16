module MyLoop(appLoop) where

import Control.Monad (unless)
import Data.IORef(IORef)
import SDL(get,($=))
import SDL.Video.Renderer (Renderer,Texture)
import SDL.Time(delay)
import MyAction(mainAction)
import MyData(State)
import MyEvent(inputEvent)

appLoop :: IORef State -> Renderer -> [Texture] -> [Texture] -> IO ()
appLoop state re ftexs itexs = do 
  st <- get state
  (nst,qPressed) <- inputEvent st
  nst' <- mainAction nst re ftexs itexs
  state $= nst'
  delay 30 
  unless qPressed (appLoop state re ftexs itexs)

