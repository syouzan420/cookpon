module MyTimer(mainTimer) where

import SDL.Video (Renderer,Texture)
import SDL.Video.Renderer (present)
import Data.IORef(IORef,readIORef,writeIORef)
import SDL.Time (TimerCallback,RetriggerTimer(Reschedule))
import MyData (State(..),initKeyEventCount)
import MyDraw (initDraw,testDraw,charaDraw)

mainTimer :: IORef State -> Renderer -> [Texture] -> [Texture] -> TimerCallback
mainTimer state re ftexs itexs i = do
  State ps kc <- readIORef state
  initDraw re
  testDraw re ftexs itexs
  charaDraw re itexs ps kc
  present re
  let nkec = if kc==0 then initKeyEventCount else kc-1
  let nst = State ps nkec 
  writeIORef state nst
  return (Reschedule i)
