module MyAction(myAction) where

import qualified Data.Text as T 
import Linear.V2(V2(..))
import MyData (State(..),Chara(..),initCharaAnimeCount,initTextEventCount,movePixel
              ,mapLimitRight,mapLimitDown,initGamePosition)

myAction :: State -> [T.Text] -> State 
myAction st tx = do
  let State (pl:cs) kc it lc ti tc _ _ (V2 sx sy) _ = st 
      Chara (V2 px py) ca cp dr bls = pl
      cps = map cpn cs
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
      ncps = if ca==0 then map (\c -> if c==0 then 1 else 0) cps else cps
      npl = pl{pos=V2 npx' npy',cac=ncac,cpn=ncp,dir=ndr}
      ncs = zipWith (\c nc -> c{cpn=nc}) cs ncps
      nst = st{chas=npl:ncs,kec=nkec,lec=nlc,tec=ntc,msc=V2 nsx nsy}
   in nst 
