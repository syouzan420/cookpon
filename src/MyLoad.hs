{-#LANGUAGE OverloadedStrings #-}
module MyLoad(loadImages,loadFonts,loadText) where

import qualified SDL.Image as I 
import qualified SDL.Font as F
import SDL(get,($=))
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import SDL.Vect(V4(..))
import SDL.Video.Renderer(Surface)
import MyData(State(..),Fchr,hiragana)
import MyFile(fileRead)

loadImages :: [FilePath] -> IO [Surface]
loadImages = mapM I.load 

loadFonts :: F.PointSize -> [FilePath] -> IO [Surface]
loadFonts fs files = do
  font <- mapM (`F.load` fs) files
  mapM (\(fnt,tx) -> F.blended fnt (V4 255 255 255 255) tx) 
                   (zip font ["abcdefghijklmnopqrstuvwxyz",hiragana,hiragana])

loadText :: F.PointSize -> Fchr -> [FilePath] -> FilePath -> State -> IO (State,[Surface])
loadText fs fc fontFiles textFile st = do
  let fontFile = fontFiles!!fromEnum fc
  font <- F.load fontFile fs
  doc <- fileRead textFile 
  let docl = map changeSpace (T.lines doc)
  docfonts <- mapM (F.blended font (V4 255 255 255 255)) docl 
  F.free font
  let nst = st{txt=docl}
  return (nst,docfonts)
  
changeSpace :: T.Text -> T.Text
changeSpace = T.replace " " "\12288"
