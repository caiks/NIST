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
import AlignmentDevRepa hiding (aahr, tframe)
import NISTDev

refr1 k (VarPair (VarPair (VarInt f, l), VarInt i)) = VarPair (VarPair (VarPair (VarPair (VarInt k, VarInt f), VarInt 0), l), VarInt i)
refr1 k (VarPair (VarPair (VarPair (VarInt f, g), l), VarInt i)) = VarPair (VarPair (VarPair (VarPair (VarInt k, VarInt f), g), l), VarInt i)
refr1 _ v = v

tframe f tt = fromJust $ transformsMapVarsFrame tt (Map.fromList $ map (\v -> (v, f v)) $ Set.toList $ tvars tt)

fframe f ff = qqff (Set.map (\tt -> tframe f tt) (ffqq ff))

fid = variablesVariableFud . least . fder

dflt df i = zzdf $ pathsTree $ Set.map (\ll -> filter (\(ss,ff) -> fid ff < VarInt i) ll) $ treesPaths $ dfzz df

decompercondrr vvl uu aa kmax omax fmax = fromJust $ parametersSystemsHistoryRepasDecomperConditionalFmaxRepa kmax omax fmax uu vvl aa

main :: IO ()
main = 
  do
    printf ">>>\n"
    hFlush stdout
 
    [valency_s,modelin,kmax_s,omax_s,fmax_s,model] <- getArgs
    let (valency,kmax,omax,fmax) = (read valency_s, read kmax_s, read omax_s, read fmax_s)

    printf "valency: %d\n" $ valency
    printf "model in: %s\n" $ modelin
    printf "model out: %s\n" $ model
    printf "kmax: %d\n" $ kmax
    printf "omax: %d\n" $ omax
    printf "fmax: %d\n" $ fmax
    hFlush stdout

    (uu,hr) <- nistTrainBucketedIO valency

    printf "train size: %d\n" $ hrsize hr
    hFlush stdout

    let digit = VarStr "digit"
    let vv = uvars uu
    let vvl = sgl digit
    let vvk = vv `minus` vvl

    df1 <- dfIO  (modelin ++ ".json")

    let uu1 = uu `uunion` (fsys (dfff df1))

    let ff1 = fframe (refr1 3) $ dfnul uu1 df1 3

    printf "model cardinality: %d\n" $ card $ fvars $ dfff df1
    hFlush stdout

    let uu1 = uu `uunion` (fsys ff1)

    printf "underlying level sys cardinality: %d\n" $ card $ uvars uu1
    hFlush stdout

    let hr1 = hrfmul uu1 ff1 hr

    printf "underlying level size: %d\n" $ hrsize hr1
    hFlush stdout

    printf ">>> %s\n" $ model
    hFlush stdout
    let (uu2,df2) = decompercondrr vvl uu1 hr1 kmax omax fmax
    let df2' = zzdf $ funcsTreesMap (\(ss,ff) -> (ss, (ff `funion` ff1) `fdep` fder ff)) $ dfzz df2
    BL.writeFile (model ++ ".json") $ decompFudsPersistentsEncode $ decompFudsPersistent df2'
    printf "<<< done %s\n" $ model
    hFlush stdout

    printf "model cardinality: %d\n" $ (Set.size $ fvars $ dfff df2')
    hFlush stdout

    let hr' = hrev [i | i <- [0.. hrsize hr - 1], i `mod` 8 == 0] hr

    printf "selected train size: %d\n" $ hrsize hr'
    hFlush stdout

    let pp = qqll $ treesPaths $ hrmult uu2 df2' hr'

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



