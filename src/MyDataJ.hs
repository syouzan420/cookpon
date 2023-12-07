module MyDataJ(T位置,Tキャラ(..),T材料(..),Tモノ(..),T地形(..),Tマス(..)
              ,chaNum,zaiNum,tikNum,Gmap,testMap,makeGmap,findCpos,findMpos
              ,delFromGmap) where

import Linear.V2(V2(..))
import Foreign.C.Types (CInt)

type T位置 = V2 CInt
data Tキャラ = Nob | Taka | Teru | Cook deriving (Eq,Enum,Bounded,Show)
data T材料 = I米 | I水 | I海苔 | I梅 deriving (Eq,Enum,Bounded,Show)
data T料理 = Fおにぎり | Fごはん deriving (Eq,Enum,Bounded,Show)
data Tモノ = No | F T料理 | I T材料 deriving (Eq,Show)
data T地形 = T道 | T壁 deriving (Eq,Enum,Bounded,Show)
data Tマス = Tマス T位置 Tモノ Tキャラ T地形 deriving (Eq,Show)

type Gmap = [[Tマス]]

chaNum :: Int
chaNum = fromEnum (maxBound :: Tキャラ) + 1

zaiNum :: Int
zaiNum = fromEnum (maxBound :: T材料) + 1

tikNum :: Int
tikNum = fromEnum (maxBound :: T地形) + 1

charToMasu :: Char -> T位置 ->  Tマス
charToMasu ch ps = 
  let (mono,chara,chikei) = case ch of
        '.' -> (No,Nob,T道); 'T' -> (No,Taka,T道); 'Y' -> (No,Teru,T道); 'C' -> (No,Cook,T道);
        'X' -> (No,Nob,T壁); 'k' -> (I I米,Nob,T道); 'n' -> (I I海苔,Nob,T道); 'm' -> (I I水,Nob,T道);
        'u' -> (I I梅,Nob,T道); 'o' -> (F Fおにぎり,Nob,T道); _ -> (No,Nob,T壁)
   in Tマス ps mono chara chikei

makeGmap :: [String] -> Gmap
makeGmap lst = zipWith (\str py->zipWith (\ch px->charToMasu ch (V2 px py)) str [(0::CInt)..]) lst [(0::CInt)..]

findMpos :: Tモノ -> Gmap -> [T位置]
findMpos _ [] = []
findMpos mon (ml:mls) = foldl (\ac (Tマス p m _ _) -> if m==mon then p:ac else ac) [] ml ++ findMpos mon mls

findCpos :: Tキャラ -> Gmap -> [T位置]
findCpos _ [] = []
findCpos car (ml:mls) = foldl (\ac (Tマス p _ c _) -> if c==car then p:ac else ac) [] ml ++ findCpos car mls

delFromGmap :: T位置 -> Gmap -> Gmap
delFromGmap ps = updateGmap ps No 

addToGmap :: T位置 -> Tモノ -> Gmap -> Gmap
addToGmap ps mono gmp = let (Tマス _ pmono _ _) = findFromPos ps gmp 
                         in if pmono==No then updateGmap ps mono gmp else gmp

findFromPos :: T位置 -> Gmap -> Tマス
findFromPos (V2 px py) gmp = (gmp!!fromIntegral py)!!fromIntegral px 

updateGmap :: T位置 -> Tモノ -> Gmap -> Gmap
updateGmap (V2 px py) mono gmp =
  let px' = fromIntegral px; py' = fromIntegral py
      (hgls,tgl:lgls) = splitAt py' gmp
      (hgs,(Tマス _ _ chara chikei):lgs) = splitAt px' tgl
      nline = hgs++[Tマス (V2 px py) mono chara chikei]++lgs 
   in hgls++[nline]++lgls 

testMap :: Gmap
testMap = makeGmap testdata

testdata :: [String] 
testdata = ["T...XX...."
           ,".XX.n..X.."
           ,"...k.X...."
           ,"mu...X.XX."
           ,"C.X......Y"]
