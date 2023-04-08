{-# LANGUAGE OverloadedStrings #-}
module MyData where

import SDL.Vect (V2(..))
import Foreign.C.Types (CInt)
import qualified Data.Text as T
import SDL.Font (PointSize) 

type Pos = V2 CInt 

data State = State{pos :: Pos, kec :: CInt, cac :: CInt, dir :: CInt}
-- pos:Position , kec:KeyEventCount , dir: Direction

data Fchr = Ro | Hi deriving Eq -- Roman, Hiragana

initState :: State
initState = State{pos=V2 100 100, kec=0, cac=10, dir=0} --kec:KeyboardEventCount

initKeyEventCount :: CInt
initKeyEventCount = charaSize `div` movePixel 

initCharaAnimeCount :: CInt
initCharaAnimeCount = 20

fontSize :: PointSize
fontSize = 24

charaSize :: CInt
charaSize = 64

movePixel :: CInt
movePixel = 4

hiragana :: T.Text
hiragana = "あかはなまいきひにみうくふぬむえけへねめおこほのもとろそよをてれせゑつるすゆんちりしゐたらさやわ"

