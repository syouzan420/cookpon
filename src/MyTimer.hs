module MyTimer(mainTimer) where

import SDL.Video (Renderer,Texture)
import SDL.Video.Renderer (present)
import Data.IORef(IORef,readIORef,writeIORef)
import SDL.Vect (V2(..))
import SDL.Time (TimerCallback,RetriggerTimer(Reschedule))
import MyData (State(..),initKeyEventCount)
import MyDraw (initDraw,testDraw,charaDraw)

mainTimer :: IORef State -> Renderer -> [Texture] -> [Texture] -> TimerCallback
mainTimer state re ftexs itexs i = do
  State ps@(V2 px py) kc dr _ <- readIORef state
  let dp = 8
  let npx = case dr of 3 -> px - dp; 7 -> px + dp; _ -> px
  let npy = case dr of 1 -> py + dp; 9 -> py - dp; _ -> py
  initDraw re
--  testDraw re ftexs itexs
  charaDraw re itexs (V2 npx npy) kc
  present re
  let nkec = if kc==0 then initKeyEventCount else kc-1
  let nst = State (V2 npx npy) nkec dr False 
  writeIORef state nst
  return (Reschedule i)
