module MySDL.MyDraw(myDraw,initDraw) where

import SDL.Video (Renderer,Texture)
import SDL.Video.Renderer (rendererDrawColor,clear,copy,copyEx,Rectangle(..),textureAlphaMod,present)
import SDL (($=))
import SDL.Vect (Point(P),V2(..),V4(..))
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as T
import Foreign.C.Types (CInt)
import MyData(State(..),Fchr(..),Pos,charaSize,fontSize,letterSize
             ,initTextPosition,initGamePosition,verticalLetterGap,horizontalLetterGap
             ,textLimitBelow,textLimitLeft,hideAlpha)
import MyDataJ(Gmap,Tマス(..),Tモノ(..),chaNum,tikNum)

myDraw :: State -> [T.Text] -> Renderer -> [Texture] -> [Texture] -> IO State
myDraw st tx re ftexs itexs = do
  let State ps _ _ cp _ it lc ti _ ts mp ms _ = st
  initDraw re
  mapDraw re (drop 4 itexs) mp ms it 
  charaDraw re (take 2 itexs) ps cp it 
  let startTextPosition = initTextPosition + V2 ts 0
  nts <- if it then textsDraw re ftexs startTextPosition ts tx ti lc 0 else return ts
  present re
  let nst = st{tsc=nts}
  return nst

initDraw :: MonadIO m => Renderer -> m ()
initDraw re = do
  rendererDrawColor re $= V4 182 100 255 255
  clear re

mapDraw :: Renderer -> [Texture] -> Gmap -> V2 CInt -> Bool -> IO ()
mapDraw re itexs mps ms it = do 
  mapM_ (\t -> textureAlphaMod t $= if it then hideAlpha else 255) itexs
  mapLinesDraw re itexs mps ms 0

mapLinesDraw :: Renderer -> [Texture] -> Gmap -> V2 CInt -> Int -> IO ()
mapLinesDraw _ _ [] _ _ = return ()
mapLinesDraw re itexs (mp:mps) ms i = do
  let position = initGamePosition + (V2 0 charaSize*fromIntegral i) - ms
  let charaSizeH = charaSize `div` 2
  let charaSizeQ = charaSize `div` 4
  mapM_ (\(Tマス (V2 x _) _ chi)-> copy re (itexs!!fromEnum chi) Nothing 
      (Just$Rectangle (P (position+V2 (charaSize*fromIntegral x) 0))(V2 charaSize charaSize))) mp
  mapM_ (\(Tマス (V2 x _) mon _)-> if monoToNum mon < (1+chaNum) 
                       then return ()
                       else copy re (itexs!!(monoToNum mon - chaNum - 1 + tikNum)) Nothing 
      (Just$Rectangle (P (position+V2 charaSizeQ 0+V2 (charaSize*fromIntegral x) charaSizeQ))(V2 charaSizeH charaSizeH))) mp
  mapLinesDraw re itexs mps ms (i+1)

monoToNum :: Tモノ -> Int
monoToNum mn = case mn of
                 No -> 0
                 C cha -> 1 + fromEnum cha
                 I zai -> 1 + chaNum + fromEnum zai

charaDraw :: Renderer -> [Texture] -> Pos -> Int -> Bool -> IO ()
charaDraw re itexs ps cp it = do
  let chara = itexs!!cp
  textureAlphaMod chara $= if it then hideAlpha else 255
  copy re chara Nothing (Just$Rectangle (P ps) (V2 charaSize charaSize))

textsDraw :: Renderer -> [Texture] -> Pos -> CInt -> [T.Text] -> Int -> Int -> Int -> IO CInt 
textsDraw re texs ps sc tx ti lc i = do
  if ti==i-1 then return sc else do
    let t = tx!!i
    let lc' = if ti==i then lc else T.length t - 1
    (nps,nsc) <- textDraw re [texs!!i,texs!!i,texs!!i] ps sc Hi t lc'
    let (nps',nsc') = nextTextPos '\n' nps nsc 
    textsDraw re texs nps' nsc' tx ti lc (i+1)


textDraw :: Renderer -> [Texture] -> Pos -> CInt -> Fchr -> T.Text -> Int -> IO (Pos,CInt)
textDraw re ftexs ps sc fc tx lc = do
  let fontIndex = case fc of Ro -> 0; Hi -> 1; Os -> 2
  showChars re (ftexs!!fontIndex) fc letterSize tx ps sc (lc+1) 0

showChars :: Renderer -> Texture -> Fchr -> CInt -> T.Text -> Pos -> CInt -> Int -> Int -> IO (Pos,CInt) 
showChars r t fc s tx p sc lc i = do
  if lc==i then return (p,sc) else do
    let ch = T.index tx i
    let (np,ns) = nextTextPos ch p sc
--    if ch=='\n' then return () else showOneChar r t fc s p ch
    if ch=='\n' then return () else showOneIndexChar r t s p ch i 
    showChars r t fc s tx np ns lc (i+1)

nextTextPos :: Char -> Pos -> CInt -> (Pos,CInt)
nextTextPos ch (V2 px py) sc =
  let dx = letterSize + horizontalLetterGap
      dy = letterSize + verticalLetterGap
      V2 _ textLimitUpper = initTextPosition
      npy = if ch=='\n' then textLimitUpper else py + dy
      npy' = if npy > textLimitBelow then textLimitUpper else npy
      npx = if npy > textLimitBelow || ch=='\n' then px - dx else px
      npx' = if npx < textLimitLeft then npx + dx else npx
      nsc = if npx < textLimitLeft then sc + dx else sc
   in (V2 npx' npy',nsc)

showOneIndexChar :: MonadIO m => Renderer -> Texture -> CInt -> Pos -> Char -> Int -> m ()
showOneIndexChar r t s p ch i =
  let wds = fromIntegral fontSize
      irt = ch=='」' || ch=='「' 
      rectSrc = Just (Rectangle (P (V2 (fromIntegral i*wds) 0)) (V2 wds wds))
      rectDst = Just (Rectangle (P p) (V2 s s))
      rotate = 90
   in if irt then copyEx r t rectSrc rectDst rotate Nothing (V2 False False)
             else copy r t rectSrc rectDst

--testDraw :: Renderer -> [Texture] -> [Texture] -> IO ()
--testDraw re ftexs itexs = do
--  rendererDrawColor re $= V4 0 0 0 255
--  drawPoint re (P (V2 100 100))
--  drawLine re (P (V2 50 50)) (P (V2 80 80))
--  let rects = map (\(V4 x y w h) -> Rectangle (P (V2 x y)) (V2 w h))
--                [V4 30 300 400 50,V4 10 200 550 30,V4 10 250 550 30,V4 10 400 30 30]
--  mapM_ (\(s,tex) -> copy re tex Nothing (Just s)) (zip rects ftexs)
--  showOneChar re (head ftexs) Ro 40 (V2 50 400) 'e'
--  showOneChar re (ftexs!!1) Hi 40 (V2 100 400) 'は'
--  showOneChar re (ftexs!!2) Hi 40 (V2 150 400) 'は'

--showOneChar :: MonadIO m => Renderer -> Texture -> Fchr -> CInt -> Pos -> Char -> m ()
--showOneChar r t fc s p ch =
--  let fchrs = case fc of
--              Ro -> ['a'..'z']
--              _ -> T.unpack hiragana
--    wds = fromIntegral fontSize
--    dx = case fc of Ro -> 14; _ -> wds 
--    dy = case fc of Ro -> wds; _ ->  wds 
--    index = fromMaybe (-1) (elemIndex ch fchrs)
-- in copy r t (Just (Rectangle (P (V2 (fromIntegral index*dx) 0)) (V2 dx dy)))
--            (Just (Rectangle (P p) (V2 s s)))

