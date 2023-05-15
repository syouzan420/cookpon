{-# LANGUAGE OverloadedStrings #-}
module MyData(Pos,Chara(..),State(..),Fchr(..),initState,initKeyEventCount,initCharaAnimeCount
             ,initTextEventCount,initTextPosition,initPlayerPosition,initGamePosition
             ,fontSize,fontColor,hideAlpha,letterSize,mapLimitRight,mapLimitDown
             ,verticalLetterGap,horizontalLetterGap,textLimitBelow,textLimitLeft,charaSize
             ,movePixel,fontFiles,imageFiles,textFiles
             ,musicFiles,title,windowSize,delayTime) where

import Linear.V2 (V2(..))
import Linear.V4 (V4(..))
import Foreign.C.Types (CInt)
import qualified Data.Text as T
import Data.Word (Word8,Word32)
import MyDataJ (Gmap,testMap,findCpos,Tキャラ(..),Tモノ(..))

type Pos = V2 CInt 
type PointSize = Int

data Chara = Chara{pos :: Pos, cac :: CInt, cpn :: Int, dir :: CInt, bel :: [Tモノ]}
-- pos: Position, cac: CharaAnimeCount, cpn: CharaPictureNumber, dir: Direction
-- bel: Belongings 

data State = State{chas :: [Chara], kec :: CInt
                  , itx :: Bool, lec :: Int, txi :: Int, tec :: CInt, tsc :: CInt
                  , gmp :: Gmap, msc :: V2 CInt, tim :: Integer}
-- kec: KeyEventCount
-- itx: Is Text On? lec: LetterCount
-- txi: TextIndex, tec: TextEventCount, tsc: TextScrollPoints
-- msc: Map Scroll

data Fchr = Ro | Hi | Os deriving (Eq,Enum) -- Roman, Hiragana, oshite

title :: T.Text
title = "COOKPON"

windowSize :: V2 CInt
windowSize = V2 480 600

initState :: State
initState = State{chas=[initPlayer,initTaka,initCook], kec=0
                 , itx=False, lec=0, txi=0, tec=3, tsc=0, gmp=testMap, msc=initScr, tim=0}                   

initPosition :: Tキャラ -> Gmap -> V2 CInt
initPosition chara gm  = initGamePosition + V2 charaSize charaSize * head (findCpos chara gm)

initPlayer :: Chara
initPlayer = Chara{pos=realPlayerPosition, cac=initCharaAnimeCount, cpn=0, dir=0}

initTaka :: Chara
initTaka = Chara{pos=initTakaPosition, cac=initCharaAnimeCount, cpn=0, dir=0}

initCook :: Chara
initCook = Chara{pos=initCookPosition, cac=initCharaAnimeCount, cpn=0, dir=0}

fontFiles :: [FilePath]
fontFiles = map ("font/"++) ["monaco.ttf","marugo.TTC","oshide.otf"]

imageFiles :: [FilePath]
imageFiles = map (\s -> "images/"++s++".png") ["teru0","teru1","takapon1","takapon2","cook5","cook6"
                                              ,"chikei0","chikei1"
                                              ,"kome","mizu","nori","ume"
                                              ,"onigiri"]

textFiles :: [FilePath]
textFiles = map ("texts/"++) ["cook0.txt","test.txt"]

musicFiles :: [FilePath]
musicFiles = ["music/test.mp3"]

delayTime :: Word32
delayTime = 50

initKeyEventCount :: CInt
initKeyEventCount = charaSize `div` movePixel

initCharaAnimeCount :: CInt
initCharaAnimeCount = 8 

initTextEventCount :: CInt
initTextEventCount = 0

initTakaPosition :: Pos 
initTakaPosition = initPosition Taka testMap

initCookPosition :: Pos 
initCookPosition = initPosition Cook testMap

initPlayerPosition :: Pos 
initPlayerPosition = initPosition Teru testMap

realPlayerPosition :: Pos 
realPlayerPosition = let (V2 initPlayerX initPlayerY) = initPlayerPosition
                         (V2 initGameX initGameY) = initGamePosition
                         realPlayerX = if initPlayerX>mapLimitRight then 
                            ((mapLimitRight-initGameX) `div` charaSize)*charaSize + initGameX else initPlayerX
                         realPlayerY = if initPlayerY>mapLimitDown then
                            ((mapLimitDown-initGameY) `div` charaSize)*charaSize + initGameY else initPlayerY
                      in V2 realPlayerX realPlayerY

initScr :: V2 CInt
initScr = let (V2 initPlayerX initPlayerY) = initPlayerPosition
              (V2 realPlayerX realPlayerY) = realPlayerPosition
              scrX = if initPlayerX>mapLimitRight then initPlayerX-realPlayerX else 0
              scrY = if initPlayerY>mapLimitDown then initPlayerY-realPlayerY else 0
           in V2 scrX scrY

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

mapLimitRight :: CInt
mapLimitRight = 380 

mapLimitDown :: CInt
mapLimitDown = 400

charaSize :: CInt
charaSize = 48 

movePixel :: CInt
movePixel = 8

