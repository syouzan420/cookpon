module MyDataJ(T位置,Tキャラ(..),T材料(..),Tモノ(..),T地形(..),Tマス(..)
              ,chaNum,zaiNum,tikNum,Gmap,testMap,makeGmap,findGpos) where

import Linear.V2(V2(..))
import Foreign.C.Types (CInt)

type T位置 = V2 CInt
data Tキャラ = Taka | Teru | Cook deriving (Eq,Enum,Bounded,Show)
data T材料 = F米 | Fおにぎり | F水 | F海苔 | F梅 deriving (Eq,Enum,Bounded,Show)
data Tモノ = No | C Tキャラ | I T材料 deriving (Eq,Show)
data T地形 = T道 | T壁 deriving (Eq,Enum,Bounded,Show)
data Tマス = Tマス T位置 Tモノ T地形 deriving (Eq,Show)

type Gmap = [[Tマス]]

chaNum :: Int
chaNum = fromEnum (maxBound :: Tキャラ) + 1

zaiNum :: Int
zaiNum = fromEnum (maxBound :: T材料) + 1

tikNum :: Int
tikNum = fromEnum (maxBound :: T地形) + 1

charToMasu :: Char -> T位置 ->  Tマス
charToMasu ch ps = 
  let (mono,chikei) = case ch of
        '.' -> (No,T道); 'T' -> (C Taka,T道); 'Y' -> (C Teru,T道); 'C' -> (C Cook,T道); 'X' -> (No,T壁)
        'k' -> (I F米,T道); 'n' -> (I F海苔,T道); 'm' -> (I F水,T道); 'u' -> (I F梅,T道)
        'o' -> (I Fおにぎり,T道); _ -> (No,T壁)
   in Tマス ps mono chikei

makeGmap :: [String] -> Gmap
makeGmap lst = zipWith (\str py->zipWith (\ch px->charToMasu ch (V2 px py)) str [(0::CInt)..]) lst [(0::CInt)..]

findGpos :: Tモノ -> Gmap -> [T位置]
findGpos _ [] = []
findGpos mon (ml:mls) = foldl (\ac (Tマス p m _) -> if m==mon then p:ac else ac) [] ml ++ findGpos mon mls

testMap :: Gmap
testMap = makeGmap testdata

testdata :: [String] 
testdata = ["T...XX...."
           ,".XX.n..X.."
           ,"...k.X...."
           ,"mu...X.XX."
           ,"C.X......Y"]
