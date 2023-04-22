module MySDL.MyLoop(myLoop) where

import Control.Monad (unless)
import Data.IORef(IORef)
import SDL(get,($=))
import SDL.Video.Renderer (Renderer,Texture)
import SDL.Time(delay)
import MyAction(myAction)
import MySDL.MyDraw(myDraw)
import qualified Data.Text as T
import MyData(State(..),delayTime)
import MyEvent(inputEvent)

myLoop :: IORef State -> [T.Text] -> Renderer -> [Texture] -> [Texture] -> IO ()
myLoop state tx re ftexs itexs = do 
  st <- get state
  let time = tim st
  (nst,qPressed) <- inputEvent st tx 
  nst' <- myDraw (myAction nst tx) tx re ftexs itexs
  state $= nst'{tim=time+1}
--  print time
  delay delayTime 
  unless qPressed (myLoop state tx re ftexs itexs)

