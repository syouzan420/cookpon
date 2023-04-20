module MyAction(myAction) where

import qualified Data.Text as T
import Linear.V2(V2(..))
import MyData (State(..),initCharaAnimeCount,initTextEventCount,movePixel)

myAction :: State -> State 
myAction st = do
  let State (V2 px py) kc ca cp dr tx it lc ti tc ts mp tm = st 
      dp = movePixel 
      ndr = if kc==0 then 0 else dr
      npx = case ndr of 3 -> px - dp; 7 -> px + dp; _ -> px
      npy = case ndr of 1 -> py + dp; 9 -> py - dp; _ -> py
      tlen = T.length (tx!!ti)
      nlc = if not it || tc>0 || lc==tlen-1 then lc else lc+1
      ntc = if tc==0 then initTextEventCount else tc-1
      nkec = if kc==0 then 0 else kc-1
      ncac = if ca==0 then initCharaAnimeCount else ca-1
      ncp = if ca==0 then if cp==0 then 1 else 0 else cp
      nst = State (V2 npx npy) nkec ncac ncp ndr tx it nlc ti ntc ts mp tm
   in nst 
