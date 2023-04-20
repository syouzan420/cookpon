{-#LANGUAGE OverloadedStrings #-}
module MyLoad(myLoad) where

import qualified SDL.Image as I 
import qualified SDL.Font as F
import qualified Data.Text as T
import SDL.Video.Renderer(Surface)
import MyData(State(..),Fchr(..),fontColor,fontSize,fontFiles,textFiles,imageFiles,initState)
import MyFile(fileRead)

myLoad :: IO (State,[Surface],[Surface])
myLoad = do
  (newState,fontTS) <- loadText fontSize Hi fontFiles (head textFiles) initState 
  imageS <- loadImages imageFiles 
  return (newState,fontTS,imageS)

loadImages :: [FilePath] -> IO [Surface]
loadImages = mapM I.load 

{--
loadFonts :: F.PointSize -> [FilePath] -> IO [Surface]
loadFonts fs files = do
  font <- mapM (`F.load` fs) files
  mapM (\(fnt,tx) -> F.blended fnt (V4 255 255 255 255) tx) 
                   (zip font ["abcdefghijklmnopqrstuvwxyz",hiragana,hiragana])
--}

loadText :: F.PointSize -> Fchr -> [FilePath] -> FilePath -> State -> IO (State,[Surface])
loadText fs fc fFiles textFile st = do
  let fontFile = fFiles!!fromEnum fc
  font <- F.load fontFile fs
  doc <- fileRead textFile 
  let docl = map changeSpace (T.lines doc)
  docfonts <- mapM (F.blended font fontColor) docl 
  F.free font
  let nst = st{txt=docl}
  return (nst,docfonts)
  
changeSpace :: T.Text -> T.Text
changeSpace = T.replace " " "\12288"
