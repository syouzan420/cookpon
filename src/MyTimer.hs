module MyTimer(mainTimer) where

import SDL.Video (Renderer,Texture)
import SDL.Video.Renderer (present)
import SDL.Vect (V2(..))
import SDL.Time (TimerCallback,RetriggerTimer(Reschedule))
import Data.IORef(IORef,readIORef,writeIORef)
import qualified Data.Text as T
import MyData (State(..),Fchr(..),initCharaAnimeCount,initTextEventCount,movePixel)
import MyDraw (initDraw,charaDraw,textDraw)

mainTimer :: IORef State -> Renderer -> [Texture] -> [Texture] -> TimerCallback
mainTimer state re ftexs itexs i = do
  State (V2 px py) kc ca dr tx lc tc <- readIORef state
  let dp = movePixel 
  let npx = case dr of 3 -> px - dp; 7 -> px + dp; _ -> px
  let npy = case dr of 1 -> py + dp; 9 -> py - dp; _ -> py
  initDraw re
  let tlen = T.length tx
  textDraw re ftexs Hi tx lc
--  testDraw re ftexs itexs
  charaDraw re itexs (V2 npx npy) ca 
  present re
  let nlc = if tc>0 || lc==tlen-1 then lc else lc+1
  let ntc = if tc==0 then initTextEventCount else tc-1
  let nkec = if kc==0 then 0 else kc-1
  let ncac = if ca==0 then initCharaAnimeCount else ca-1
  let ndr = if kc==0 then 0 else dr
  let nst = State (V2 npx npy) nkec ncac ndr tx nlc ntc
  writeIORef state nst
  return (Reschedule i)
