{-# LANGUAGE OverloadedStrings #-}
module MyData(Pos,State(..),Fchr(..),initState,initKeyEventCount,initCharaAnimeCount
             ,initTextEventCount,initTextPosition,initPlayerPosition,initGamePosition
             ,fontSize,fontColor,hideAlpha,letterSize
             ,verticalLetterGap,horizontalLetterGap,textLimitBelow,textLimitLeft,charaSize
             ,movePixel,hiragana,fontFiles,imageFiles,textFiles
             ,musicFiles,title,windowSize,testMap) where

import SDL.Vect (V4(..),V2(..))
import Foreign.C.Types (CInt)
import SDL.Font (PointSize) 
import qualified Data.Text as T
import Data.Word (Word8)

type Pos = V2 CInt 
type Gmap = [[Int]]

data State = State{pos :: Pos, kec :: CInt, cac :: CInt, cpn :: Int, dir :: CInt
                  ,txt :: [T.Text], itx :: Bool, lec :: Int, txi :: Int, tec :: CInt, tsc :: CInt, gmp :: Gmap}
-- pos: Position , kec: KeyEventCount
-- cac: CharaAnimeCount, cpn: CharaPictureNumber
-- dir: Direction
-- itx: Is Text On? lec: LetterCount
-- txi: TextIndex, tec: TextEventCount, tsc: TextScrollPoints

data Fchr = Ro | Hi | Os deriving (Eq,Enum) -- Roman, Hiragana, oshite

title :: T.Text
title = "COOKPON"

windowSize :: V2 CInt
windowSize = V2 480 600

initState :: State
initState = State{pos=initPlayerPosition, kec=0, cac=initCharaAnimeCount, cpn=0, dir=0 
                 ,txt=[], itx=False, lec=0, txi=0, tec=3, tsc=0, gmp=testMap}                   

fontFiles :: [FilePath]
fontFiles = map ("font/"++) ["monaco.ttf","marugo.TTC","oshide.otf"]

imageFiles :: [FilePath]
imageFiles = map ("images/"++) ["cook5.png","cook6.png","takapon1.png","takapon2.png"]

textFiles :: [FilePath]
textFiles = map ("texts/"++) ["cook0.txt","cook0.txt"]

musicFiles :: [FilePath]
musicFiles = ["music/cooktest3.mp3"]


initKeyEventCount :: CInt
initKeyEventCount = charaSize `div` movePixel

initCharaAnimeCount :: CInt
initCharaAnimeCount = 8 

initTextEventCount :: CInt
initTextEventCount = 0

initPlayerPosition :: V2 CInt
initPlayerPosition = V2 70 70

initGamePosition :: V2 CInt
initGamePosition = V2 70 70

initTextPosition :: V2 CInt
initTextPosition = V2 400 20

fontSize :: PointSize
fontSize = 24

fontColor :: V4 Word8
fontColor = V4 255 255 204 255

hideAlpha :: Word8
hideAlpha =50 

letterSize :: CInt
letterSize = 30

verticalLetterGap :: CInt
verticalLetterGap = 2

horizontalLetterGap :: CInt
horizontalLetterGap = 10

textLimitBelow :: CInt
textLimitBelow = 500

textLimitLeft :: CInt
textLimitLeft = 50

charaSize :: CInt
charaSize = 48 

movePixel :: CInt
movePixel = 8

hiragana :: T.Text
hiragana = "あかはなまいきひにみうくふぬむえけへねめおこほのもとろそよをてれせゑつるすゆんちりしゐたらさやわがばぎびぐぶげべごぼどぞでぜづずぢじだざぱぴぷぺぽっアカハナマイキヒニミウクフヌムエケヘネメオコホノモトロソヨヲテレセヱツルスユンチリシヰタラサヤワガバギビグブゲベゴボドゾデゼヅズヂジダザパピプペポッ"

testMap :: [[Int]]
testMap = [[0,0,1,1,1],[1,0,0,1,1],[1,0,0,1,1],[1,1,1,0,0],[1,0,1,0,1]]
