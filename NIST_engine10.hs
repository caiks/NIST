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

    (uu,hrtr) <- nistTrainBucketedRegionRandomIO 2 11 17

    let u = stringsVariable "<6,6>"

    let hr = hrtr `hrhrsel` aahr uu (unit (sgl (llss [(u,ValInt 1)])))

    printf "selected train size: %d\n" $ hrsize hr
    hFlush stdout

    let digit = VarStr "digit"
    let vv = uvars uu
    let vvl = sgl digit
    let vvk = vv `minus` vvl

    let model = "NIST_model10"
    let (wmax,lmax,xmax,omax,bmax,mmax,umax,pmax,fmax,mult,seed) = (2^11, 8, 2^10, 30, (30*3), 3, 2^8, 1, 127, 1, 5)

    printf ">>> %s\n" $ model
    hFlush stdout
    Just (uu1,df1) <- decomperIO uu vvk hr wmax lmax xmax omax bmax mmax umax pmax fmax mult seed
    let w = stringsVariable "<<0,1>,1>"
    let ff0 = qqff $ sgl $ trans (unit (llqq [llss [(u,ValInt 0),(w,ValInt 0)], llss [(u,ValInt 1),(w,ValInt 1)]])) (sgl w)
    let ss0 = llss [(w,ValInt 1)]
    let df1' = lldf $ map (\((_,ff1):ll) -> (stateEmpty,ff0):((ss0,ff1):ll)) $ dfll df1
    BL.writeFile (model ++ ".json") $ decompFudsPersistentsEncode $ decompFudsPersistent df1'
    printf "<<< done %s\n" $ model
    hFlush stdout

    printf "model cardinality: %d\n" $ (Set.size $ fvars $ dfff df1')
    hFlush stdout

    let hr' = hrev [i | i <- [0.. hrsize hrtr - 1], i `mod` 8 == 0] hrtr

    printf "selected train size: %d\n" $ hrsize hr'
    hFlush stdout

    let uu1 = uu `uunion` (fsys (dfff df1'))

    let (a,ad) = summation mult seed uu1 df1' hr'
    printf "alignment: %.2f\n" $ a
    printf "alignment density: %.2f\n" $ ad
    hFlush stdout

    let pp = qqll $ treesPaths $ hrmult uu1 df1' hr'

    bmwrite (model ++ ".bmp") $ bmvstack $ map (\bm -> bminsert (bmempty ((11*3)+2) (((11*3)+2)*(maximum (map length pp)))) 0 0 bm) $ map (bmhstack . map (\(_,hrs) -> bmborder 1 (hrbm 11 3 2 (hrs `hrhrred` vvk)))) $ pp
    printf "bitmap %s\n" $ model ++ ".bmp"
    hFlush stdout

    bmwrite (model ++ "_1.bmp") $ bmvstack $ map (\bm -> bminsert (bmempty (((11*3)*1)+2) ((((11*3)*1)+2)*(maximum (map length pp)))) 0 0 bm) $ map (bmhstack . map (\((_,ff),hrs) -> bmborder 1 (bmmax (hrbm 11 3 2 (hrs `hrhrred` vvk)) 0 0 (hrbm 11 3 2 (qqhr 2 uu vvk (fund ff)))))) $ pp
    printf "bitmap %s\n" $ model ++ "_1.bmp"
    hFlush stdout

    bmwrite (model ++ "_2.bmp") $ bmvstack $ map (\bm -> bminsert (bmempty (((11*2)*2)+2) ((((11*2)*2)+2)*(maximum (map length pp)))) 0 0 bm) $ map (bmhstack . map (\((_,ff),hrs) -> bmborder 1 (bmmax (hrbm 11 (2*2) 2 (hrs `hrhrred` vvk)) 0 0 (hrbm 11 (2*2) 2 (qqhr 2 uu vvk (fund ff)))))) $ pp
    printf "bitmap %s\n" $ model ++ "_2.bmp"
    hFlush stdout

    printf "<<< done\n"


