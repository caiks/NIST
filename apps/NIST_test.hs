{-# LANGUAGE DeriveGeneric, OverloadedStrings, BangPatterns, ScopedTypeVariables #-}

module NIST_test ( main) where

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
import AlignmentDevRepa
import NISTDev

amax = llaa . take 1 . reverse . map (\(a,b) -> (b,a)) . sort . map (\(a,b) -> (b,a)) . aall . norm . trim

main :: IO ()
main = 
  do
    printf ">>>\n"
    hFlush stdout
 
    [model] <- getArgs

    printf "model: %s\n" $ model
    hFlush stdout

    (uu,hrtr) <- nistTrainBucketedIO 2

    let hr = hrev [i | i <- [0.. hrsize hrtr - 1], i `mod` 8 == 0] hrtr 

    printf "selected train size: %d\n" $ hrsize hr
    hFlush stdout

    let hrtr = undefined

    let digit = VarStr "digit"
    let vv = uvars uu
    let vvl = sgl digit
    let vvk = vv `minus` vvl

    s <- BL.readFile (model ++ ".json")
    let df1 = fromJust $ persistentsDecompFud $ fromJust $ (Data.Aeson.decode s :: Maybe DecompFudPersistent)

    printf "model cardinality: %d\n" $ card $ fvars $ dfff df1
    hFlush stdout

    let uu1 = uu `uunion` (fsys (dfff df1))

    let ff1 = fromJust $ systemsDecompFudsNullablePracticable uu1 df1 9

    printf "nullable fud cardinality: %d\n" $ card $ fvars $ ff1
    hFlush stdout

    printf "nullable fud derived cardinality: %d\n" $ card $ fder $ ff1
    hFlush stdout

    printf "nullable fud underlying cardinality: %d\n" $ card $ fund $ ff1
    hFlush stdout

    let uu1 = uu `uunion` (fsys ff1)

    let hr1 = hrfmul uu1 ff1 hr

    printf "ff label ent: %.16f\n" $ hrlent uu1 hr1 (fder ff1) vvl
    hFlush stdout

    (uu,hrte) <- nistTestBucketedIO 2

    let hrq = hrev [i | i <- [0 .. hrsize hrte - 1], i `mod` 10 == 0] hrte 

    printf "test size: %d\n" $ hrsize hrq
    hFlush stdout

    let hrte = undefined

    let hrq1 = hrfmul uu1 ff1 hrq

    printf "effective size: %s\n" $ rp $ size $ hhaa (hrhh uu1 (hrq1 `hrhrred` (fder ff1))) `mul` eff (hhaa (hrhh uu1 (hr1 `hrhrred` (fder ff1))))
    hFlush stdout

    printf "matches: %d\n" $ length [rr | let hhq = hrhh uu1 (hrq1 `hrhrred` (fder ff1 `union` vvl)), let aa = hhaa (hrhh uu1 (hr1 `hrhrred` (fder ff1 `union` vvl))), (_,ss) <- hhll hhq, let qq = single ss 1, let rr = aa `mul` (qq `red` fder ff1) `red` vvl, size rr > 0, size (amax rr `mul` (qq `red` vvl)) > 0]
    hFlush stdout

    printf "<<< done\n"



