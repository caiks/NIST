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
import System.Environment
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
import AlignmentDevRepa hiding (aahr, tframe, qqff, funion, hrfmul)
import NISTDev

hrfmul = systemsFudsHistoryRepasMultiply_u

amax = llaa . take 1 . reverse . map (\(a,b) -> (b,a)) . sort . map (\(a,b) -> (b,a)) . aall . norm . trim

main :: IO ()
main = 
  do
    [model] <- getArgs

    printf "model: %s\n" $ model
    hFlush stdout

    (uu,hrtr) <- nistTrainBucketedIO 2

    let hr = hrev [i | i <- [0.. hrsize hrtr - 1], i `mod` 8 == 0] hrtr 

    printf "train size: %d\n" $ hrsize hr
    hFlush stdout

    let hrtr = undefined

    let digit = VarStr "digit"
    let vv = uvars uu
    let vvl = sgl digit
    let vvk = vv `minus` vvl

    s <- BL.readFile (model ++ ".json")
    let df = fromJust $ persistentsDecompFud $ fromJust $ (Data.Aeson.decode s :: Maybe DecompFudPersistent)

    printf "df cardinality: %d\n" $ card $ fvars $ dfff df
    hFlush stdout

    let uu1 = uu `uunion` (fsys (dfff df))

    bmwrite "NIST.bmp" $ bmvstack $ map (\bm -> bminsert (bmempty ((28*1)+2) (((28*1)+2)*9)) 0 0 bm) $ map (bmhstack . map (\((_,ff),hrs) -> bmborder (28*1) (hrbm (28*1) 28 1 0 2 (hrs `hrhrred` vvk)))) $ qqll $ treesPaths $ hrmult uu1 df hr

    let ff = fromJust $ systemsDecompFudsNullablePracticable uu1 df 2

    printf "ff cardinality: %d\n" $ card $ fvars $ ff
    hFlush stdout

    printf "ff derived cardinality: %d\n" $ card $ fder $ ff
    hFlush stdout

    printf "ff underlying cardinality: %d\n" $ card $ fund $ ff
    hFlush stdout

    let uu2 = uu `uunion` (fsys ff)

    let hrb = hrfmul uu2 ff hr

    printf "train multiplied size: %d\n" $ hrsize hrb
    hFlush stdout

    printf "ff label ent: %.20f\n" $ hrlent uu2 hrb (fder ff) vvl
    hFlush stdout

    (uu,hrte) <- nistTestBucketedIO 2

    let hrq = hrev [i | i <- [0 .. hrsize hrte - 1], i `mod` 10 == 0] hrte 

    printf "test size: %d\n" $ hrsize hrq
    hFlush stdout

    let hrte = undefined

    let hrqb = hrfmul uu2 ff hrq

    printf "test multiplied size: %d\n" $ hrsize hrqb
    hFlush stdout

    printf "effective size: %s\n" $ rp $ size $ hhaa (hrhh uu2 (hrqb `hrhrred` (fder ff))) `mul` eff (hhaa (hrhh uu2 (hrb `hrhrred` (fder ff))))
    hFlush stdout

    printf "matches: %d\n" $ length [rr | let hhq = hrhh uu2 (hrqb `hrhrred` (fder ff `union` vvl)), let aa = hhaa (hrhh uu2 (hrb `hrhrred` (fder ff `union` vvl))), (_,ss) <- hhll hhq, let qq = single ss 1, let rr = aa `mul` (qq `red` fder ff) `red` vvl, size rr > 0, size (amax rr `mul` (qq `red` vvl)) > 0]
    hFlush stdout

    printf "done\n"


