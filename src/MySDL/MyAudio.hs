module MySDL.MyAudio(myAudio) where

import qualified SDL.Mixer as M
import MyData (musicFiles)

myAudio :: IO ()
myAudio = do
  M.openAudio M.defaultAudio 256
  M.load (head musicFiles) >>= M.playMusic M.Forever
  au <- M.queryAudio
  cd <- M.chunkDecoders
  md <- M.musicDecoders
  print M.defaultAudio
  print au
  print cd
  print md

--testAudio :: M.Audio
--testAudio = M.Audio 44100 M.FormatS16_LSB M.Stereo
