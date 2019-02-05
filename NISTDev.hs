{-# LANGUAGE DeriveGeneric, OverloadedStrings, BangPatterns, FlexibleContexts #-}

module NISTDev
where

import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.List hiding (union)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Shape as RS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BL8
import qualified Codec.Compression.GZip as GZip
import Data.Array.Repa.Repr.ByteString
import Data.Array.Repa.IO.BMP
import Data.Word
import Data.Aeson hiding (Value)
import System.Random
import System.IO
import Data.Time
import System.Locale
import Text.Printf
import GHC.Real

import AlignmentUtil
import Alignment
import AlignmentApprox
import AlignmentRandom
import AlignmentPracticable
import AlignmentPracticableIO
import AlignmentAeson
import AlignmentAesonPretty
import AlignmentRepaVShape
import AlignmentRepa
import AlignmentAesonRepa
import AlignmentRandomRepa
import AlignmentPracticableRepa
import AlignmentPracticableIORepa
import AlignmentDevRepa hiding (aahr)

bmempty sx sy = R.computeS $ R.fromFunction ((R.Z R.:. sx R.:. sy) :: R.DIM2) (const (0,0,0)) :: R.Array R.U R.DIM2 (Word8, Word8, Word8)

bminsert bm2 ox oy bm1 = bm :: R.Array R.U R.DIM2 (Word8, Word8, Word8)
  where
    bm = R.computeS $ R.traverse2 bm2 bm1 (\_ _ -> R.extent bm2) ins
    (R.Z R.:. sx R.:. sy) = R.extent bm1
    ins f1 f2 (R.Z R.:. x R.:. y) = 
      if (x>=ox && x<ox+sx && y>=oy && y<oy+sy) 
        then (f2 (R.Z R.:. (x-ox) R.:. (y-oy))) 
        else (f1 (R.Z R.:. x R.:. y))

bmborder b bm = bminsert bm2 1 1 bm 
  where
    bm2 = R.computeS $ R.fromFunction ((R.Z R.:. (b+2) R.:. (b+2)) :: R.DIM2) ins :: R.Array R.U R.DIM2 (Word8, Word8, Word8)
    ins (R.Z R.:. x R.:. y) = 
      if (x==0 || x==b+1 || y==0 || y==b+1) 
        then (255,255,255) 
        else (0,0,0)

hrbm a b c q d hr = bminsert bm2 q q bm1 :: R.Array R.U R.DIM2 (Word8, Word8, Word8)
  where
    z = hrsize hr
    ar1 = R.sumS $ historyRepasArray hr
    ar2 = R.map (\x -> x * 255 `div` (d-1) `div` z) ar1
    ar3 = R.reshape ((R.Z R.:. b R.:. b) :: R.DIM2) ar2
    ar4 = R.backpermute ((R.Z R.:. b R.:. b) :: R.DIM2) flip ar3
    flip (R.Z R.:. x R.:. y) = (R.Z R.:. (b-1-x) R.:. y)
    ar5 = R.backpermute ((R.Z R.:. (b*c) R.:. (b*c)) :: R.DIM2) expand ar4
    expand  (R.Z R.:. x R.:. y) = (R.Z R.:. (x`div`c) R.:. (y`div`c))
    bm1 = R.computeS $ R.map (\x -> let y = fromInteger (toInteger x) in (y,y,y)) ar5 :: R.Array R.U R.DIM2 (Word8, Word8, Word8)
    bm2 = R.computeS $ R.fromFunction ((R.Z R.:. a R.:. a) :: R.DIM2) (const (0,0,0)) :: R.Array R.U R.DIM2 (Word8, Word8, Word8)

hrllbmh a b c q d ll hh = bm
  where
    vvk = Set.fromList [VarPair (VarInt (toInteger i),VarInt (toInteger j)) | i <- [1..b], j <- [1..b]]
    lla = zip [0..] [hrev [i] hh | i <- ll]
    sy = (length ll) * (a+1) + 1
    sx = a + 2
    bm = foldl (\bm2 (x,hr) -> bminsert bm2 0 (x * (a+1)) (bmborder a (hrbm a b c q d (hr `hrhrred` vvk)))) (bmempty sx sy) lla

hrllbmv a b c q d ll hh = bm
  where
    vvk = Set.fromList [VarPair (VarInt (toInteger i),VarInt (toInteger j)) | i <- [1..b], j <- [1..b]]
    lla = zip [0..] [hrev [i] hh | i <- ll]
    sx = (length ll) * (a+1) + 1
    sy = a + 2
    bm = foldl (\bm2 (x,hr) -> bminsert bm2 (sx - ((x+1) * (a+1) + 1)) 0 (bmborder a (hrbm a b c q d (hr `hrhrred` vvk)))) (bmempty sx sy) lla

lluu ll = fromJust $ listsSystem [(v, llqq ww) | (v,ww) <- ll]

aahr uu aa = hhhr uu $ aahh aa 

decomperIO uu vv hh wmax lmax xmax omax bmax mmax umax pmax fmax mult seed =
      parametersSystemsHistoryRepasDecomperMaxRollByMExcludedSelfHighestFmaxIORepa 
        wmax lmax xmax omax bmax mmax umax pmax fmax mult seed uu vv hh

nistTrainIO :: IO (System, HistoryRepa)
nistTrainIO = 
    do
      let z = 60000 :: Int
      lz <- BL.readFile "./train-labels-idx1-ubyte.gz"
      let l = (R.copyS $ R.map (fromInteger . toInteger) $ fromByteString ((R.Z R.:. z R.:. 1) :: R.DIM2) $ BL.toStrict $ BL.drop 8 $ GZip.decompress lz) :: R.Array R.U R.DIM2 Int
      pz <- BL.readFile "./train-images-idx3-ubyte.gz"
      let p = (R.copyS $ R.map (fromInteger . toInteger) $ fromByteString ((R.Z R.:. z R.:. (28*28)) :: R.DIM2) $ BL.toStrict $ BL.drop 16 $ GZip.decompress pz) :: R.Array R.U R.DIM2 Int
      let h = (R.computeS $ l R.++ p) :: R.Array R.U R.DIM2 Int
      let uu = lluu $ [(VarStr "digit", [ValInt i | i <- [0..9]])] ++ [( VarPair (VarInt x, VarInt y), [ValInt i | i <- [0..255]]) | x <- [1..28], y <- [1..28]]
      let vv = qqll (uvars uu)
      let svv = (UV.fromList $ [10] ++ replicate (28*28) 256) :: VShape
      let hr = HistoryRepa (V.fromListN (28*28+1) vv) (Map.fromList (zip vv [0..])) svv $ R.computeS $ R.transpose h
      return (uu,hr)

nistTestIO :: IO (System, HistoryRepa)
nistTestIO = 
    do
      let z = 10000 :: Int
      lz <- BL.readFile "./t10k-labels-idx1-ubyte.gz"
      let l = (R.copyS $ R.map (fromInteger . toInteger) $ fromByteString ((R.Z R.:. z R.:. 1) :: R.DIM2) $ BL.toStrict $ BL.drop 8 $ GZip.decompress lz) :: R.Array R.U R.DIM2 Int
      pz <- BL.readFile "./t10k-images-idx3-ubyte.gz"
      let p = (R.copyS $ R.map (fromInteger . toInteger) $ fromByteString ((R.Z R.:. z R.:. (28*28)) :: R.DIM2) $ BL.toStrict $ BL.drop 16 $ GZip.decompress pz) :: R.Array R.U R.DIM2 Int
      let h = (R.computeS $ l R.++ p) :: R.Array R.U R.DIM2 Int
      let uu = lluu $ [(VarStr "digit", [ValInt i | i <- [0..9]])] ++ [( VarPair (VarInt x, VarInt y), [ValInt i | i <- [0..255]]) | x <- [1..28], y <- [1..28]]
      let vv = qqll (uvars uu)
      let svv = (UV.fromList $ [10] ++ replicate (28*28) 256) :: VShape
      let hr = HistoryRepa (V.fromListN (28*28+1) vv) (Map.fromList (zip vv [0..])) svv $ R.computeS $ R.transpose h
      return (uu,hr)

