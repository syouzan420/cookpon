module MyAction(myAction) where

import qualified Data.Text as T 
import Linear.V2(V2(..))
import MyData (State(..),initCharaAnimeCount,initTextEventCount,movePixel
              ,mapLimitRight,mapLimitDown,initGamePosition)

myAction :: State -> [T.Text] -> State 
myAction st tx = do
  let State (V2 px py) kc ca cp dr it lc ti tc _ _ (V2 sx sy) _ = st 
      dp = movePixel 
      ndr = if kc==0 then 0 else dr
      npx = case ndr of 3 -> px - dp; 7 -> px + dp; _ -> px
      npy = case ndr of 1 -> py + dp; 9 -> py - dp; _ -> py
      V2 mapLimitLeft mapLimitUp = initGamePosition
      imlr = npx > mapLimitRight; imld = npy > mapLimitDown
      imll = npx < mapLimitLeft; imlu = npy < mapLimitUp
      nsx
        | imlr = sx+dp
        | imll = sx-dp 
        | otherwise = sx 
      nsy
        | imld = sy+dp
        | imlu = sy-dp
        | otherwise = sy
      npx' = if imlr || imll then px else npx
      npy' = if imld || imlu then py else npy
      tlen = T.length (tx!!ti)
      nlc = if not it || tc>0 || lc==tlen-1 then lc else lc+1
      ntc = if tc==0 then initTextEventCount else tc-1
      nkec = if kc==0 then 0 else kc-1
      ncac = if ca==0 then initCharaAnimeCount else ca-1
      ncp = if ca==0 then if cp==0 then 1 else 0 else cp
      nst = st{pos=V2 npx' npy',kec=nkec,cac=ncac,cpn=ncp,dir=ndr,lec=nlc,tec=ntc,msc=V2 nsx nsy}
   in nst 
