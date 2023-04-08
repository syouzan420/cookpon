module MyDraw(initDraw,charaDraw) where


import SDL.Video (Renderer,Texture)
import SDL.Video.Renderer (rendererDrawColor,clear,drawPoint,drawLine,copy,Rectangle(..))
import SDL (($=))
import SDL.Vect (Point(P),V2(..),V4(..))
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as T
import Foreign.C.Types (CInt)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import MyData(Fchr(..),Pos,initKeyEventCount,initCharaAnimeCount,charaSize,hiragana,fontSize)

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

charaDraw :: Renderer -> [Texture] -> Pos -> CInt -> IO ()
charaDraw re itexs ps ca = do
  let chara = if ca>(initCharaAnimeCount `div` 2) then head itexs else itexs!!1
  copy re chara Nothing (Just$Rectangle (P ps) (V2 charaSize charaSize))

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

