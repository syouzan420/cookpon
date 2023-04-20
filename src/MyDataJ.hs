module MyDataJ(T位置,T材料(..),Tモノ(..),T地形(..),Tマス,Gmap,testMap) where

import Linear.V2(V2(..))

type T位置 = V2 Int
data T材料 = F米 | Fおにぎり | F水 | F海苔 | F梅 deriving (Eq,Enum,Show)
data Tモノ = No | Taka | Teru | Cook | I T材料 deriving (Eq,Show)
data T地形 = T道 | T壁 deriving (Eq,Enum,Show)
data Tマス = Tマス T位置 Tモノ T地形 deriving (Eq,Show)

type Gmap = [[Tマス]]

charToMasu :: Char -> T位置 ->  Tマス
charToMasu ch ps = 
  let (mono,chikei) = case ch of
        '.' -> (No,T道); 'T' -> (Taka,T道); 'Y' -> (Teru,T道); 'C' -> (Cook,T道); 'X' -> (No,T壁)
        'k' -> (I F米,T道); 'n' -> (I F海苔,T道); 'm' -> (I F水,T道); 'u' -> (I F梅,T道)
        'o' -> (I Fおにぎり,T道); _ -> (No,T壁)
   in Tマス ps mono chikei

makeGmap :: [String] -> Gmap
makeGmap lst = zipWith (\str py->zipWith (\ch px->charToMasu ch (V2 px py)) str [(0::Int)..]) lst [(0::Int)..]

testMap :: Gmap
testMap = makeGmap testdata

testdata :: [String] 
testdata = ["T...."
           ,".XX.n"
           ,"...k."
           ,"mu..."
           ,"C.X.Y"]
