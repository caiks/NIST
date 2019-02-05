{-# LANGUAGE DeriveGeneric, OverloadedStrings, BangPatterns, ScopedTypeVariables #-}


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
import Data.Word
import Data.Aeson hiding (Value)
import System.Random
import System.IO
import Data.Time
import System.Locale
import Text.Printf
import GHC.Real
import Data.Csv
import Control.Applicative
import GHC.Generics

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
import NISTDev

main :: IO ()
main = 
  do
    printf ">>>\n"
    hFlush stdout

    csvtr <- BL.readFile "train.csv"
    let vvcsvtr = either (\_ -> V.empty) id (Data.Csv.decode HasHeader csvtr :: Either String (V.Vector Train))
    let aatr = llaa [(llss [(VarStr s, fw rr) | (s,fw) <- trmap],1) | rr <- V.toList vvcsvtr]

    csvte <- BL.readFile "test.csv"
    let vvcsvte = either (\_ -> V.empty) id (Data.Csv.decode HasHeader csvte :: Either String (V.Vector Test))
    let aate = llaa [(llss [(VarStr s, fw rr) | (s,fw) <- temap],1) | rr <- V.toList vvcsvte]

    let uu = sys aatr `uunion` sys aate
    let vv = uvars uu `minus` sgl (VarStr "Id")
    let vvl = sgl (VarStr "SalePrice")
    let vvk = vv `minus` vvl

    let aa = (aatr `red` vvk) `add` (aate `red` vvk)

    let vvo = llqq [w | w <- qqll vv, isOrd uu w, let u = vol uu (sgl w), u > 16]

    let vvoz = llqq [w | w <- qqll vv, isOrd uu w, let u = vol uu (sgl w), u > 16, let rr = unit (sgl (llss [(w, ValInt 0)])), let bb = aatr `red` sgl w `mul` rr, size bb > 100]

    let xx = Map.fromList $ map (\(v,ww) -> let VarStr s = v in (v, (VarStr (s ++ "B"), ww))) $ [(v, bucket 20 aa v) | v <- qqll (vvo `minus` vvoz)] ++ [(VarStr "SalePrice", bucket 20 aatr (VarStr "SalePrice"))] ++ [(v, bucket 20 aa' v) | v <- qqll vvoz, let rr = unit (sgl (llss [(v, ValInt 0)])), let bb = aa `red` sgl v `mul` rr, let aa' = trim (aa `red` sgl v `sub` bb)]

    let aab = reframeb aa xx
    let aatrb = reframeb aatr xx

    let uub = sys aab `uunion` sys aatrb
    let vvb = uvars uub `minus` sgl (VarStr "Id")
    let vvbl = sgl (VarStr "SalePriceB")
    let vvbk = vvb `minus` vvbl

    let hhb = aahr uub aab `hrhrred` vvbk

    printf "train size: %d\n" $ hrsize hhb
    hFlush stdout

    let model = "NIST_model1"
    let (wmax,lmax,xmax,omax,bmax,mmax,umax,pmax,fmax,mult,seed) = (2919, 8, 2919, 50, (50*5), 5, 2919, 1, 20, 10, 5)

    printf ">>> %s\n" $ model
    Just (uub',dfb') <- decomperIO uub vvbk hhb wmax lmax xmax omax bmax mmax umax pmax fmax mult seed
    BL.writeFile (model ++ ".json") $ decompFudsPersistentsEncode $ decompFudsPersistent dfb'
    printf "<<< done %s\n" $ model
    hFlush stdout

    printf "decomp size: %d\n" $ (Set.size $ fvars $ dfff dfb')

    let (a,ad) = summation mult seed uub' dfb' hhb
    printf "alignment: %.2f\n" $ a
    printf "alignment density: %.2f\n" $ ad

    printf "<<< done\n"
  where 
    summation = systemsDecompFudsHistoryRepasAlignmentContentShuffleSummation_u

