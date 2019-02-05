{-# LANGUAGE DeriveGeneric, OverloadedStrings, BangPatterns #-}

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

lluu ll = fromJust $ listsSystem [(v, llqq ww) | (v,ww) <- ll]

nistTrainIO :: IO (System, HistoryRepa)
nistTrainIO = 
    do
      let z = 60000 :: Int
      lz <- BL.readFile "./train-labels-idx1-ubyte.gz"
      let l = (R.copyS $ R.map (fromInteger . toInteger) $ fromByteString ((R.Z R.:. z R.:. 1) :: R.DIM2) $ BL.toStrict $ BL.drop 8 $ GZip.decompress lz) :: R.Array R.U R.DIM2 Int
      pz <- BL.readFile "./train-images-idx3-ubyte.gz"
      let p = (R.copyS $ R.map (fromInteger . toInteger) $ fromByteString ((R.Z R.:. z R.:. (28*28)) :: R.DIM2) $ BL.toStrict $ BL.drop 16 $ GZip.decompress pz) :: R.Array R.U R.DIM2 Int
      let b = 28 :: Int
      let c = 1 :: Int
      let q = 0 :: Int
      let d = 2 :: Int
      let r = R.reshape ((R.Z R.:. z R.:. (b*b)) :: R.DIM2) $ R.map (\x -> x*d`div`(c*c*256)) $ R.sumS $ R.backpermute ((R.Z R.:. z R.:. b R.:. b R.:. (c*c)) :: R.DIM4) ((\(R.Z R.:. u R.:. x1 R.:. x2 R.:. x3) -> (R.Z R.:. u R.:. x1*c+q+(x3`div`c) R.:. x2*c+q+(x3`mod`c))) :: R.DIM4 -> R.DIM3) $ R.reshape ((R.Z R.:. z R.:. 28 R.:. 28) :: R.DIM3) p
      let h = (R.computeS $ l R.++ r) :: R.Array R.U R.DIM2 Int
      let uu = fromJust $ systemFromList $ [(VarStr "digit", Set.fromList [ValInt i | i <- [0..9]])] ++ [( VarPair (VarInt (toInteger x), VarInt (toInteger y)), Set.fromList [ValInt (toInteger i) | i <- [1..d]]) | x <- [1..b], y <- [1..b]]
      let vv = [VarStr "digit"] ++ [VarPair (VarInt (toInteger x), VarInt (toInteger y)) | x <- [1..b], y <- [1..b]]
      let svv = (UV.fromList $ [10] ++ replicate (b*b) d) :: VShape
      let hr = HistoryRepa (V.fromListN (b*b+1) vv) (Map.fromList (zip vv [0..])) svv $ R.computeS $ R.transpose h
      return (uu,hr)

aahr uu aa = hhhr uu $ aahh aa 

decomperIO uu vv hh wmax lmax xmax omax bmax mmax umax pmax fmax mult seed =
      parametersSystemsHistoryRepasDecomperMaxRollByMExcludedSelfHighestFmaxIORepa 
        wmax lmax xmax omax bmax mmax umax pmax fmax mult seed uu vv hh



