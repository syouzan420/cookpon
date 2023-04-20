module MySDL.MyQuit (myQuit) where

--import SDL.Mixer (closeAudio)
import SDL.Video (Window,destroyWindow)

myQuit :: Window -> IO ()
myQuit window = do
  --closeAudio
  destroyWindow window
