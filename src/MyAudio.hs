module MyAudio(myAudio) where

import qualified SDL.Mixer as M
import MyData (musicFiles)

myAudio :: IO ()
myAudio = do
  M.openAudio M.defaultAudio 256
  M.load (head musicFiles) >>= M.playMusic M.Forever
  cd <- M.chunkDecoders
  md <- M.musicDecoders
  print cd
  print md

--testAudio :: M.Audio
--testAudio = M.Audio 11025 M.FormatS16_Sys M.Stereo
