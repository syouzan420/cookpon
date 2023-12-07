module MyEvent(inputEvent) where

import Linear.V2 (V2(..))
import qualified Data.Text as T
import Foreign.C.Types (CInt)
import MyData (Chara(..),State(..),initKeyEventCount,initGamePosition,charaSize)
import MySDL.MyInput (myInput)
import MyDataJ (Gmap,Tマス(..),T地形(..),Tモノ(..),delFromGmap)

inputEvent :: State -> [T.Text] -> IO (State,Bool)
inputEvent st tx = do
  let State cha kc it lc ti _ _ mp (V2 sx sy) _ = st
  let pl = head cha
  let belPl = bel pl
  let Chara (V2 px py) _ _ dr _ = pl 
  [qPressed,spReleased,hPressed,jPressed,kPressed,lPressed] <- myInput 
  let len = length tx
  let tlen = T.length (tx!!ti)
  let (V2 lfp upp) = initGamePosition
  let (V2 gx gy) = V2 ((round$(fromIntegral (px+sx-lfp)::Float) / fromIntegral charaSize)::CInt) 
                      (round$(fromIntegral (py+sy-upp)::Float) / fromIntegral charaSize)
  let iLeftEdge = gx == 0 
  let iUpEdge = gy == 0 
  let mapHLength = length (head mp)
  let mapVLength = length mp
  let iRightEdge = gx == fromIntegral mapHLength - 1 
  let iDownEdge = gy == fromIntegral mapVLength - 1 
  let iCanGet = length belPl < 2 
  
  let onFloor = findMono (V2 gx gy) mp 
  let nmp 
        | onFloor == No = mp
        | iCanGet       = delFromGmap (V2 gx gy) mp
        | otherwise     = mp 

  let nbel = if iCanGet && onFloor /= No then belPl ++ [onFloor] else belPl 

  if belPl /= nbel then print nbel else return ()

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
      npl = pl{dir=ndr,bel=nbel}
      st' = st{chas=npl:tail cha,kec=nkec,itx=nit,lec=nlec,txi=nti,gmp=nmp}
  return (st',qPressed)

isPass :: V2 CInt -> Gmap -> Bool
isPass (V2 gx gy) mp = let ln = mp!!fromIntegral gy
                           tg = ln!!fromIntegral gx
                           Tマス _ _ _ chi = tg
                        in chi==T道

findMono :: V2 CInt -> Gmap -> Tモノ 
findMono (V2 gx gy) mp = let ln = mp!!fromIntegral gy 
                             tg = ln!!fromIntegral gx
                             Tマス _ mono _ _ = tg
                          in mono
