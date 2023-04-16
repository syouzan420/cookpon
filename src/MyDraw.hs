module MyDraw(initDraw,charaDraw,textDraw,textsDraw,mapDraw) where


import SDL.Video (Renderer,Texture)
import SDL.Video.Renderer (rendererDrawColor,clear,copy,Rectangle(..))
import SDL (($=))
import SDL.Vect (Point(P),V2(..),V4(..))
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as T
import Foreign.C.Types (CInt)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import MyData(Fchr(..),Pos,initCharaAnimeCount,charaSize,hiragana,fontSize,letterSize
             ,initTextPosition,initGamePosition,verticalLetterGap,horizontalLetterGap
             ,textLimitBelow,textLimitLeft)

initDraw :: Renderer -> IO ()
initDraw re = do
  rendererDrawColor re $= V4 182 100 255 255
  clear re

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

mapDraw :: Renderer -> [Texture] -> [[Int]] -> IO ()
mapDraw re itexs mps = mapLinesDraw re itexs mps 0

mapLinesDraw :: Renderer -> [Texture] -> [[Int]] -> Int -> IO ()
mapLinesDraw _ _ [] _ = return ()
mapLinesDraw re itexs (mp:mps) i = do
  let position = initGamePosition + (V2 0 charaSize*fromIntegral i)
  mapM_ (\(c,x)-> copy re (itexs!!c) Nothing 
      (Just$Rectangle (P (position+V2 (charaSize*fromIntegral x) 0))(V2 charaSize charaSize))) (zip mp [0..])
  mapLinesDraw re itexs mps (i+1)

charaDraw :: Renderer -> [Texture] -> Pos -> Int -> IO ()
charaDraw re itexs ps cp = do
  let chara = itexs!!cp
  copy re chara Nothing (Just$Rectangle (P ps) (V2 charaSize charaSize))

textsDraw :: Renderer -> [Texture] -> Pos -> CInt -> [T.Text] -> Int -> Int -> Int -> IO CInt 
textsDraw re texs ps sc tx ti lc i = do
  if ti==i-1 then return sc else do
    let txt = tx!!i
    let lc' = if ti==i then lc else T.length txt - 1
    (nps,nsc) <- textDraw re [texs!!i,texs!!i,texs!!i] ps sc Hi txt lc'
    let (nps',nsc') = nextTextPos '\n' nps nsc 
    textsDraw re texs nps' nsc' tx ti lc (i+1)


textDraw :: Renderer -> [Texture] -> Pos -> CInt -> Fchr -> T.Text -> Int -> IO (Pos,CInt)
textDraw re ftexs ps sc fc txt lc = do
  let fontIndex = case fc of Ro -> 0; Hi -> 1; Os -> 2
  showChars re (ftexs!!fontIndex) fc letterSize txt ps sc (lc+1) 0

showChars :: Renderer -> Texture -> Fchr -> CInt -> T.Text -> Pos -> CInt -> Int -> Int -> IO (Pos,CInt) 
showChars r t fc s txt p sc lc i = do
  if lc==i then return (p,sc) else do
    let ch = T.index txt i
    let (np,ns) = nextTextPos ch p sc
--    if ch=='\n' then return () else showOneChar r t fc s p ch
    if ch=='\n' then return () else showOneIndexChar r t s p i 
    showChars r t fc s txt np ns lc (i+1)

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
   in (V2 npx npy',nsc)

showOneChar :: MonadIO m => Renderer -> Texture -> Fchr -> CInt -> Pos -> Char -> m ()
showOneChar r t fc s p ch =
  let fchrs = case fc of
                Ro -> ['a'..'z']
                _ -> T.unpack hiragana
      wds = fromIntegral fontSize
      dx = case fc of Ro -> 14; _ -> wds 
      dy = case fc of Ro -> wds; _ ->  wds 
      index = fromMaybe (-1) (elemIndex ch fchrs)
   in copy r t (Just (Rectangle (P (V2 (fromIntegral index*dx) 0)) (V2 dx dy)))
               (Just (Rectangle (P p) (V2 s s)))

showOneIndexChar :: MonadIO m => Renderer -> Texture -> CInt -> Pos -> Int -> m ()
showOneIndexChar r t s p i =
  let wds = fromIntegral fontSize
   in copy r t (Just (Rectangle (P (V2 (fromIntegral i*wds) 0)) (V2 wds wds)))
               (Just (Rectangle (P p) (V2 s s)))
