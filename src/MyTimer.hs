module MyTimer(mainTimer) where

import SDL.Video (Renderer,Texture)
import SDL.Video.Renderer (present)
import Data.IORef(IORef,readIORef,writeIORef)
import SDL.Vect (V2(..))
import SDL.Time (TimerCallback,RetriggerTimer(Reschedule))
import MyData (State(..),initCharaAnimeCount,movePixel)
import MyDraw (initDraw,charaDraw)

mainTimer :: IORef State -> Renderer -> [Texture] -> [Texture] -> TimerCallback
mainTimer state re ftexs itexs i = do
  State (V2 px py) kc ca dr <- readIORef state
  let dp = movePixel 
  let npx = case dr of 3 -> px - dp; 7 -> px + dp; _ -> px
  let npy = case dr of 1 -> py + dp; 9 -> py - dp; _ -> py
  initDraw re
--  testDraw re ftexs itexs
  charaDraw re itexs (V2 npx npy) ca 
  present re
  let nkec = if kc==0 then 0 else kc-1
  let ncac = if ca==0 then initCharaAnimeCount else ca-1
  let ndr = if kc==0 then 0 else dr
  let nst = State (V2 npx npy) nkec ncac ndr 
  writeIORef state nst
  return (Reschedule i)
