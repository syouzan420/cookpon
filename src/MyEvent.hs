module MyEvent(inputEvent) where

import SDL.Vect (V2(..))
import qualified Data.Text as T
import MyData (State(..),initKeyEventCount,initGamePosition,charaSize)
import MyInput (myInput)

inputEvent :: State -> IO (State,Bool)
inputEvent st = do
  [qPressed,spReleased,hPressed,jPressed,kPressed,lPressed] <- myInput 
  let (V2 px py) = pos st
  let mp = gmp st
  let kc = kec st
  let dr = dir st
  let tx = txt st
  let it = itx st
  let lc = lec st
  let ti = txi st
  let len = length tx
  let tlen = T.length (tx!!ti)
  let (V2 lfp upp) = initGamePosition
  let iLeftEdge = lfp >= px 
  let iUpEdge = upp >= py
  let mapHLength = fromIntegral$length (head mp)
  let mapVLength = fromIntegral$length mp
  let iRightEdge = lfp+(mapHLength-1)*charaSize <= px
  let iDownEdge = upp+(mapVLength-1)*charaSize <= py
  let ndr
        | kc==0 && not iLeftEdge && hPressed = 3
        | kc==0 && not iRightEdge && lPressed = 7
        | kc==0 && not iDownEdge && jPressed = 1
        | kc==0 && not iUpEdge && kPressed = 9
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
      st' = st{kec=nkec,dir=ndr,itx=nit,lec=nlec,txi=nti}
  return (st',qPressed)

