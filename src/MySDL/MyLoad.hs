{-#LANGUAGE OverloadedStrings #-}
module MySDL.MyLoad(myLoad) where

import qualified SDL.Image as I 
import qualified SDL.Font as F
import qualified Data.Text as T
import SDL.Video.Renderer(Surface)
import MyData(Fchr(..),fontColor,fontSize,fontFiles,textFiles,imageFiles)
import MyFile(fileRead)

myLoad :: Int -> IO ([T.Text],([Surface],[Surface]))
myLoad i = do
  (docl,fontTS) <- loadText fontSize Hi fontFiles (textFiles!!i)
  imageS <- loadImages imageFiles 
  return (docl,(fontTS,imageS))

loadImages :: [FilePath] -> IO [Surface]
loadImages = mapM I.load 

{--
loadFonts :: F.PointSize -> [FilePath] -> IO [Surface]
loadFonts fs files = do
  font <- mapM (`F.load` fs) files
  mapM (\(fnt,tx) -> F.blended fnt (V4 255 255 255 255) tx) 
                   (zip font ["abcdefghijklmnopqrstuvwxyz",hiragana,hiragana])
--}

loadText :: F.PointSize -> Fchr -> [FilePath] -> FilePath -> IO ([T.Text],[Surface])
loadText fs fc fFiles textFile = do
  let fontFile = fFiles!!fromEnum fc
  font <- F.load fontFile fs
  doc <- fileRead textFile 
  let docl = map changeSpace (T.lines doc)
  docfonts <- mapM (F.blended font fontColor) docl 
  F.free font
  return (docl,docfonts)
  
changeSpace :: T.Text -> T.Text
changeSpace = T.replace " " "\12288"
