module MyAction(mainAction) where

import SDL.Video (Renderer,Texture)
import SDL.Video.Renderer (present)
import SDL.Vect (V2(..))
import qualified Data.Text as T
import MyData (State(..),initCharaAnimeCount,initTextEventCount,initTextPosition,movePixel)
import MyDraw (initDraw,charaDraw,textsDraw,mapDraw)

mainAction :: State -> Renderer -> [Texture] -> [Texture] -> IO State 
mainAction st re ftexs itexs = do
  let State (V2 px py) kc ca cp dr tx lc ti tc ts mp = st 
  let startTextPosition = initTextPosition + V2 ts 0
  let dp = movePixel 
  let ndr = if kc==0 then 0 else dr
  let npx = case ndr of 3 -> px - dp; 7 -> px + dp; _ -> px
  let npy = case ndr of 1 -> py + dp; 9 -> py - dp; _ -> py
  let tlen = T.length (tx!!ti)
  let nlc = if tc>0 || lc==tlen-1 then lc else lc+1
  let ntc = if tc==0 then initTextEventCount else tc-1
  let nkec = if kc==0 then 0 else kc-1
  let ncac = if ca==0 then initCharaAnimeCount else ca-1
  let ncp = if ca==0 then if cp==0 then 1 else 0 else cp
  initDraw re
  mapDraw re (drop 2 itexs) mp 
  charaDraw re (take 2 itexs) (V2 npx npy) ncp 
  nts <- textsDraw re ftexs startTextPosition ts tx ti lc 0
  present re
  let nst = State (V2 npx npy) nkec ncac ncp ndr tx nlc ti ntc nts mp
  return nst 
