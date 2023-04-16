{-# LANGUAGE OverloadedStrings #-}
module MyData(Pos,State(..),Fchr(..),initState,initKeyEventCount,initCharaAnimeCount
             ,initTextEventCount,initTextPosition,fontSize,letterSize,verticalLetterGap
             ,horizontalLetterGap,textLimitBelow,textLimitLeft,charaSize,movePixel,hiragana
             ,testText,timerInterval,fontFiles,imageFiles,textFiles,musicFiles,title,windowSize) where

import SDL.Vect (V2(..))
import Foreign.C.Types (CInt)
import SDL.Font (PointSize) 
import qualified Data.Text as T
import Data.Word (Word32)

type Pos = V2 CInt 

data State = State{pos :: Pos, kec :: CInt, cac :: CInt, cpn :: Int, dir :: CInt
                  ,txt :: [T.Text], lec :: Int, txi :: Int, tec :: CInt, tsc :: CInt}
-- pos: Position , kec: KeyEventCount
-- cac: CharaAnimeCount, cpn: CharaPictureNumber
-- dir: Direction
-- lec: LetterCount
-- txi: TextIndex, tec: TextEventCount, tsc: TextScrollPoints

data Fchr = Ro | Hi | Os deriving (Eq,Enum) -- Roman, Hiragana, oshite

title :: T.Text
title = "COOKPON"

windowSize :: V2 CInt
windowSize = V2 480 600

initState :: State
initState = State{pos=V2 70 70, kec=0, cac=10, cpn=0, dir=0 
                 ,txt=[testText], lec=0, txi=0, tec=3, tsc=0}                   

fontFiles :: [FilePath]
fontFiles = map ("font/"++) ["monaco.ttf","marugo.TTC","oshide.otf"]

imageFiles :: [FilePath]
imageFiles = map ("images/"++) ["cook5.png","cook6.png"]

textFiles :: [FilePath]
textFiles = map ("texts/"++) ["cook0.txt","test.txt"]

musicFiles :: [FilePath]
musicFiles = ["music/cooktest3.mp3"]

timerInterval :: Word32
timerInterval = 50 

initKeyEventCount :: CInt
initKeyEventCount = charaSize `div` movePixel 

initCharaAnimeCount :: CInt
initCharaAnimeCount = 10

initTextEventCount :: CInt
initTextEventCount = 1

initTextPosition :: V2 CInt
initTextPosition = V2 400 20

fontSize :: PointSize
fontSize = 24

letterSize :: CInt
letterSize = 30

verticalLetterGap :: CInt
verticalLetterGap = 2

horizontalLetterGap :: CInt
horizontalLetterGap = 4

textLimitBelow :: CInt
textLimitBelow = 500

textLimitLeft :: CInt
textLimitLeft = 50

charaSize :: CInt
charaSize = 64

movePixel :: CInt
movePixel = 8

hiragana :: T.Text
hiragana = "あかはなまいきひにみうくふぬむえけへねめおこほのもとろそよをてれせゑつるすゆんちりしゐたらさやわがばぎびぐぶげべごぼどぞでぜづずぢじだざぱぴぷぺぽっアカハナマイキヒニミウクフヌムエケヘネメオコホノモトロソヨヲテレセヱツルスユンチリシヰタラサヤワガバギビグブゲベゴボドゾデゼヅズヂジダザパピプペポッ"

testText :: T.Text
testText = "てるすけは けふもごきげん\n\nだけど おなかをすかせた たかぽんが なんかほしいと おそってくる\n\nクックポンで なにか つくらないと"

