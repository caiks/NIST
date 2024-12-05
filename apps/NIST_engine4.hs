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

    (uu,hr) <- nistTrainBucketedIO 2

    printf "train size: %d\n" $ hrsize hr
    hFlush stdout

    let digit = VarStr "digit"
    let vv = uvars uu
    let vvl = sgl digit
    let vvk = vv `minus` vvl

    let model = "NIST_model4"
    let (wmax,lmax,xmax,omax,bmax,mmax,umax,pmax,fmax,mult,seed) = (2^11, 8, 2^10, 60, (60*3), 3, 2^8, 1, 127, 1, 5)

    printf ">>> %s\n" $ model
    Just (uu1,df1) <- decomperIO uu vvk hr wmax lmax xmax omax bmax mmax umax pmax fmax mult seed
    BL.writeFile (model ++ ".json") $ decompFudsPersistentsEncode $ decompFudsPersistent df1
    printf "<<< done %s\n" $ model
    hFlush stdout

    printf "model cardinality: %d\n" $ (Set.size $ fvars $ dfff df1)

    let hr' = hrev [i | i <- [0.. hrsize hr - 1], i `mod` 8 == 0] hr

    printf "selected train size: %d\n" $ hrsize hr'
    hFlush stdout

    let (a,ad) = summation mult seed uu1 df1 hr'
    printf "alignment: %.2f\n" $ a
    printf "alignment density: %.2f\n" $ ad
    hFlush stdout

    let pp = qqll $ treesPaths $ hrmult uu1 df1 hr'

    bmwrite (model ++ ".bmp") $ bmvstack $ map (\bm -> bminsert (bmempty (28+2) ((28+2)*(maximum (map length pp)))) 0 0 bm) $ map (bmhstack . map (\(_,hrs) -> bmborder 1 (hrbm 28 1 2 (hrs `hrhrred` vvk)))) $ pp
    printf "bitmap %s\n" $ model ++ ".bmp"
    hFlush stdout

    bmwrite (model ++ "_1.bmp") $ bmvstack $ map (\bm -> bminsert (bmempty ((28*1)+2) (((28*1)+2)*(maximum (map length pp)))) 0 0 bm) $ map (bmhstack . map (\((_,ff),hrs) -> bmborder 1 (bmmax (hrbm 28 1 2 (hrs `hrhrred` vvk)) 0 0 (hrbm 28 1 2 (qqhr 2 uu vvk (fund ff)))))) $ pp
    printf "bitmap %s\n" $ model ++ "_1.bmp"
    hFlush stdout

    bmwrite (model ++ "_2.bmp") $ bmvstack $ map (\bm -> bminsert (bmempty ((28*2)+2) (((28*2)+2)*(maximum (map length pp)))) 0 0 bm) $ map (bmhstack . map (\((_,ff),hrs) -> bmborder 1 (bmmax (hrbm 28 2 2 (hrs `hrhrred` vvk)) 0 0 (hrbm 28 2 2 (qqhr 2 uu vvk (fund ff)))))) $ pp
    printf "bitmap %s\n" $ model ++ "_2.bmp"
    hFlush stdout

    printf "<<< done\n"


