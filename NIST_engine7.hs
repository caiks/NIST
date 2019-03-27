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
import AlignmentDevRepa hiding (aahr, tframe)
import NISTDev

refr1 k (VarPair (VarPair (VarInt f, l), i)) = VarPair (VarPair (VarPair (VarInt k, VarInt f), l), i)
refr1 _ v = v

tframe f tt = fromJust $ transformsMapVarsFrame tt (Map.fromList $ map (\v -> (v, f v)) $ Set.toList $ tvars tt)

fframe f ff = qqff (Set.map (\tt -> tframe f tt) (ffqq ff))

decompercondrr vvl uu aa kmax omax fmax = fromJust $ parametersSystemsHistoryRepasDecomperConditionalFmaxRepa kmax omax fmax uu vvl aa

main :: IO ()
main = 
  do
    printf ">>>\n"
    hFlush stdout

    (uu,hr) <- nistTrainBucketedRegionRandomIO 2 10 17

    printf "train size: %d\n" $ hrsize hr
    hFlush stdout

    let digit = VarStr "digit"
    let vv = uvars uu
    let vvl = sgl digit
    let vvk = vv `minus` vvl

    s <- BL.readFile "./NIST_model6.json"
    let df1 = fromJust $ persistentsDecompFud $ fromJust $ (Data.Aeson.decode s :: Maybe DecompFudPersistent)

    let uu1 = uu `uunion` (fsys (dfff df1))

    let ff1 = fframe (refr1 1) $ fromJust $ systemsDecompFudsNullablePracticable uu1 df1 1

    let uu1 = uu `uunion` (fsys ff1)

    printf "underlying level sys cardinality: %d\n" $ card $ uvars uu1
    hFlush stdout

    let hr1 = hrfmul uu1 ff1 hr

    printf "underlying level size: %d\n" $ hrsize hr1
    hFlush stdout

    let model = "NIST_model7"
    let (kmax,omax) = (1, 5)
    let (mult,seed) = (1, 5)

    printf ">>> %s\n" $ model
    let (uu2,df2) = decompercondrr vvl uu1 (hr1 `hrhrred` (fvars ff1 `minus` vvk `union` vvl)) kmax omax 15
    let df2' = zzdf $ funcsTreesMap (\(ss,ff) -> (ss, (ff `funion` ff1) `fdep` fder ff)) $ dfzz df2
    BL.writeFile (model ++ ".json") $ decompFudsPersistentsEncode $ decompFudsPersistent df2'
    printf "<<< done %s\n" $ model
    hFlush stdout

    printf "model cardinality: %d\n" $ (Set.size $ fvars $ dfff df2')

    let hr' = hrev [i | i <- [0.. hrsize hr - 1], i `mod` 8 == 0] hr

    printf "selected train size: %d\n" $ hrsize hr'
    hFlush stdout
{-
    let (a,ad) = summation mult seed uu2 df2' hr'
    printf "alignment: %.2f\n" $ a
    printf "alignment density: %.2f\n" $ ad
    hFlush stdout
-}
    bmwrite (model ++ ".bmp") $ bmvstack $ map (\bm -> bminsert (bmempty ((10*3)+2) (((10*3)+2)*5)) 0 0 bm) $ map (bmhstack . map (\(_,hrs) -> bmborder (10*3) (hrbm (10*3) 10 3 0 2 (hrs `hrhrred` vvk)))) $ qqll $ treesPaths $ hrmult uu2 df2' hr'
    printf "bitmap %s\n" $ model ++ ".bmp"
    hFlush stdout

    printf "<<< done\n"


