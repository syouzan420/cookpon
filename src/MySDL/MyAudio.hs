module MySDL.MyAudio(myAudio,withMyAudio) where

import qualified SDL.Mixer as M
import Control.Monad.IO.Class (MonadIO)
import Control.Monad (void)
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

withMyAudio :: MonadIO m => m a -> m ()
withMyAudio op = do 
  M.openAudio M.defaultAudio 1024  
  M.load (head musicFiles) >>= M.playMusic M.Forever
  void op
  M.closeAudio
  M.quit
  
--testAudio :: M.Audio
--testAudio = M.Audio 44100 M.FormatS16_LSB M.Stereo
