module MyEvent(inputEvent) where

import Linear.V2 (V2(..))
import qualified Data.Text as T
import MyData (Chara(..),State(..),initKeyEventCount,initGamePosition,charaSize)
import MySDL.MyInput (myInput)
import MyDataJ (Gmap,Tマス(..),T地形(..))

inputEvent :: State -> [T.Text] -> IO (State,Bool)
inputEvent st tx = do
  let State cha kc it lc ti _ _ mp (V2 sx sy) _ = st
  let pl = head cha
  let Chara (V2 px py) _ _ dr = pl 
  [qPressed,spReleased,hPressed,jPressed,kPressed,lPressed] <- myInput 
  let len = length tx
  let tlen = T.length (tx!!ti)
  let (V2 lfp upp) = initGamePosition
  let (V2 gx gy) = V2 ((round$(fromIntegral (px+sx-lfp)::Float) / fromIntegral charaSize)::Int) 
                      (round$(fromIntegral (py+sy-upp)::Float) / fromIntegral charaSize)
  let iLeftEdge = gx == 0 
  let iUpEdge = gy == 0 
  let mapHLength = length (head mp)
  let mapVLength = length mp
  let iRightEdge = gx == mapHLength - 1 
  let iDownEdge = gy == mapVLength - 1 
  let ndr
        | kc==0 && not iLeftEdge && hPressed && isPass (V2 (gx-1) gy) mp = 3
        | kc==0 && not iRightEdge && lPressed && isPass (V2 (gx+1) gy) mp = 7
        | kc==0 && not iDownEdge && jPressed  && isPass (V2 gx (gy+1)) mp = 1
        | kc==0 && not iUpEdge && kPressed && isPass (V2 gx (gy-1)) mp = 9
        | kc==0 = 0
        | otherwise = dr
  let nkec = if kc==0 && ndr/=0 then initKeyEventCount else kc
  let bNewLine = it && spReleased && (tlen==lc+1 && len-1>ti)
  let nit 
        | not it && spReleased = True
        | it && len-1==ti && spReleased = False
        | otherwise = it
  let nti = if bNewLine then ti+1 else ti 
      nlec = if bNewLine then 0 else lc
      npl = pl{dir=ndr}
      st' = st{chas=npl:tail cha,kec=nkec,itx=nit,lec=nlec,txi=nti}
  return (st',qPressed)

isPass :: V2 Int -> Gmap -> Bool
isPass (V2 gx gy) mp = let ln = mp!!gy
                           tg = ln!!gx
                           Tマス _ _ _ chi = tg
                        in chi==T道
