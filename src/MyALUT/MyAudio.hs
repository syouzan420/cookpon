module MyALUT.MyAudio(withMyAudio) where

import Sound.ALUT.Initialization (withProgNameAndArgs,runALUTUsingCurrentContext)
import Sound.ALUT.Loaders (createBuffer,SoundDataSource(File))
--import Sound.ALUT.Sleep (sleep)
import Sound.OpenAL.AL.Source (loopingMode,LoopingMode(..),queueBuffers,play)
import Sound.OpenAL.ALC.Device (openDevice,closeDevice)
import Sound.OpenAL.ALC.Context (createContext,currentContext)
import SDL.Video.Renderer (Renderer,Texture)
import Data.IORef(IORef)
import Data.ObjectName (genObjectName)
import Data.StateVar (($=))
import Control.Monad.IO.Class (MonadIO)

withMyAudio :: (MonadIO m,MonadFail m)  => m a -> m ()
withMyAudio op =
  withProgNameAndArgs runALUTUsingCurrentContext $ \_ _ ->
    do
      (Just device) <- openDevice Nothing
      (Just context) <- createContext device []
      currentContext $= Just context
      buffer <- createBuffer $ File "./music/cooktest3.wav"
      source <- genObjectName
      loopingMode source $= Looping
      queueBuffers source [buffer]
      play [source]
      _ <- op 
      _ <- closeDevice device
      return ()
