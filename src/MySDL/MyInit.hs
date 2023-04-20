module MySDL.MyInit(myInit) where

import SDL.Init (initializeAll) 
import qualified SDL.Font as F
import qualified SDL.Image as I

myInit :: IO ()
myInit = do
  initializeAll
  F.initialize
  I.initialize []

