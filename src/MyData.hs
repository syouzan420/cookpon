{-# LANGUAGE OverloadedStrings #-}
module MyData where

import SDL.Vect (V2(..))
import Foreign.C.Types (CInt)
import qualified Data.Text as T
import SDL.Font (PointSize) 

type Pos = V2 CInt 

data State = State{pos:: Pos, kec:: CInt}

data Fchr = Ro | Hi deriving Eq -- Roman, Hiragana

initState :: State
initState = State{pos=V2 100 100, kec=0} --kec:KeyboardEventCount

initKeyEventCount :: CInt
initKeyEventCount = 10

fontSize :: PointSize
fontSize = 24

hiragana :: T.Text
hiragana = "あかはなまいきひにみうくふぬむえけへねめおこほのもとろそよをてれせゑつるすゆんちりしゐたらさやわ"

