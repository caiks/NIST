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

    (uu,hrtr) <- nistTrainBucketedIO 2

    printf "train size: %d\n" $ hrsize hrtr
    hFlush stdout

    let digit = VarStr "digit"
    let vv = uvars uu
    let vvl = sgl digit
    let vvk = vv `minus` vvl

    let model = "NIST_model3"
    let (wmax,lmax,xmax,omax,bmax,mmax,umax,pmax,fmax,mult,seed) = (2^11, 8, 2^11, 40, (40*5), 5, 2^8, 1, 127, 1, 5)

    printf ">>> %s\n" $ model
    Just (uu1,df) <- decomperIO uu vvk hrtr wmax lmax xmax omax bmax mmax umax pmax fmax mult seed
    BL.writeFile (model ++ ".json") $ decompFudsPersistentsEncode $ decompFudsPersistent df
    printf "<<< done %s\n" $ model
    hFlush stdout

    printf "decomp size: %d\n" $ (Set.size $ fvars $ dfff df)

    let (a,ad) = summation mult seed uu1 df hr
    printf "alignment: %.2f\n" $ a
    printf "alignment density: %.2f\n" $ ad

    printf "<<< done\n"


