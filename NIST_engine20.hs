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
import NISTDev hiding (decomperIO)

refr1 k (VarPair (VarPair (VarInt f, l), i)) = VarPair (VarPair (VarPair (VarPair (VarInt k, VarInt f), VarInt 0), l), i)
refr1 _ v = v

refr2 x y (VarPair (VarInt i,VarInt j)) = VarPair (VarInt ((x-1)+i),VarInt ((y-1)+j))
refr2 x y (VarPair (VarPair (VarPair (VarPair (VarInt k, VarInt f), VarInt g), l), i)) = (VarPair (VarPair (VarPair (VarPair (VarPair (VarStr ("(" ++ show x ++ ";" ++ show y ++ ")"),VarInt k), VarInt f), VarInt g), l), i))
refr2 _ _ v = v

tframe f tt = fromJust $ transformsMapVarsFrame tt (Map.fromList $ map (\v -> (v, f v)) $ Set.toList $ tvars tt)

fframe f ff = qqff (Set.map (\tt -> tframe f tt) (ffqq ff))

fid = variablesVariableFud . least . fder

dflt df i = zzdf $ pathsTree $ Set.map (\ll -> filter (\(ss,ff) -> fid ff < VarInt i) ll) $ treesPaths $ dfzz df

dfred = systemsDecompFudsHistoryRepasDecompFudReduced

dfnul uu df g = fromJust $ systemsDecompFudsNullablePracticable uu df g

decompercondrr vvl uu aa kmax omax fmax = fromJust $ parametersSystemsHistoryRepasDecomperConditionalFmaxRepa kmax omax fmax uu vvl aa

decomperIO uu ff aa wmax lmax xmax omax bmax mmax umax pmax fmax mult seed = parametersSystemsHistoryRepasDecomperLevelMaxRollByMExcludedSelfHighestFmaxIORepa wmax lmax xmax omax bmax mmax umax pmax fmax mult seed uu (Tree $ Map.singleton (wmax,Set.empty,ff) emptyTree) aa

main :: IO ()
main = 
  do
    printf ">>>\n"
    hFlush stdout

    (uu,hr) <- nistTrainBucketedRegionRandomIO 2 10 17

    printf "underlying region train size: %d\n" $ hrsize hr
    hFlush stdout

    s <- BL.readFile "./NIST_model6.json"
    let df1 = fromJust $ persistentsDecompFud $ fromJust $ (Data.Aeson.decode s :: Maybe DecompFudPersistent)

    let uu1 = uu `uunion` (fsys (dfff df1))

    let ff1 = fframe (refr1 1) $ dfnul uu1 (dfred uu1 df1 hr) 1

    (uu,hr) <- nistTrainBucketedRegionRandomIO 2 15 17

    printf "region train size: %d\n" $ hrsize hr
    hFlush stdout

    let digit = VarStr "digit"
    let vv = uvars uu
    let vvl = sgl digit
    let vvk = vv `minus` vvl

    let gg1 = foldl funion fudEmpty [fframe (refr2 x y) ff1 | (x,y) <- [(1,1),(6,1),(1,6),(6,6),(3,3)]]

    printf "underlying level cardinality: %d\n" $ card $ fvars gg1
    hFlush stdout

    let uu1 = uu `uunion` (fsys gg1)

    printf "underlying level sys cardinality: %d\n" $ card $ uvars uu1
    hFlush stdout

    let model = "NIST_model20"
    let (wmax,lmax,xmax,omax,bmax,mmax,umax,pmax,fmax,mult,seed) = (2^11, 8, 2^9, 30, (30*3), 3, 2^8, 1, 127, 1, 5)

    printf ">>> %s\n" $ model
    Just (uu2,df2) <- decomperIO uu1 gg1 hr wmax lmax xmax omax bmax mmax umax pmax fmax mult seed
    BL.writeFile (model ++ ".json") $ decompFudsPersistentsEncode $ decompFudsPersistent df2
    printf "<<< done %s\n" $ model
    hFlush stdout

    printf "model cardinality: %d\n" $ (Set.size $ fvars $ dfff df2)

    let hr' = hrev [i | i <- [0.. hrsize hr - 1], i `mod` 8 == 0] hr

    printf "selected train size: %d\n" $ hrsize hr'
    hFlush stdout

    let (a,ad) = summation mult seed uu2 df2 hr'
    printf "alignment: %.2f\n" $ a
    printf "alignment density: %.2f\n" $ ad
    hFlush stdout

    let pp = qqll $ treesPaths $ hrmult uu2 df2 hr'
    bmwrite (model ++ ".bmp") $ bmvstack $ map (\bm -> bminsert (bmempty (15+2) ((15+2)*(maximum (map length pp)))) 0 0 bm) $ map (bmhstack . map (\(_,hrs) -> bmborder 1 (hrbm 15 1 2 (hrs `hrhrred` vvk)))) $ pp
    printf "bitmap %s\n" $ model ++ ".bmp"
    hFlush stdout

    printf "<<< done\n"


