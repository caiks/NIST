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
import AlignmentDevRepa hiding (aahr, tframe, qqff, funion, hrfmul)
import NISTDev

qqff = setTransformsFud_u

funion ff gg = qqff (ffqq ff `Set.union` ffqq gg)

refr1 k (VarPair (VarPair (VarInt f, l), i)) = VarPair (VarPair (VarPair (VarInt k, VarInt f), l), i)

refr1 _ v = v

refr2 x y (VarPair (VarInt i,VarInt j)) = VarPair (VarInt ((x-1)+i),VarInt ((y-1)+j))

refr2 x y (VarPair (VarPair (VarPair (VarInt k, VarInt f), l), i)) = (VarPair (VarPair (VarPair (VarPair (VarStr ("(" ++ show x ++ ";" ++ show y ++ ")"),VarInt k), VarInt f), l), i))

refr2 _ _ v = v

tframe f tt = fromJust $ transformsMapVarsFrame tt (Map.fromList $ map (\v -> (v, f v)) $ Set.toList $ tvars tt)

fframe f ff = qqff (Set.map (\tt -> tframe f tt) (ffqq ff))

decompercondrr vvl uu aa kmax omax fmax = fromJust $ parametersSystemsHistoryRepasDecomperConditionalFmaxRepa kmax omax fmax uu vvl aa

hrfmul = systemsFudsHistoryRepasMultiply_u

main :: IO ()
main = 
  do
    printf ">>>\n"
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

    let model = "NIST_model8"

    s <- BL.readFile "NIST_model7.json"
    let ff2' = fromJust $ persistentsFud $ fromJust $ (Data.Aeson.decode s :: Maybe FudPersistent)

    let gg1 = foldl funion fudEmpty [fframe (refr2 x y) ff2' | x <- [1,3,5,7,9,11,13,15,17,19], y <- [1,3,5,7,9,11,13,15,17,19]] 

    printf "underlying fud cardinality: %d\n" $ card $ fvars gg1
    hFlush stdout

    let uu1 = uu `uunion` (fsys gg1)

    printf "sys cardinality: %d\n" $ card $ uvars uu1
    hFlush stdout

    let hr1 = hrfmul uu1 gg1 hr

    printf "hr1 size: %d\n" $ hrsize hr1
    hFlush stdout

    let (kmax,omax) = (1, 5)

    let (uu2,df2) = decompercondrr vvl uu1 hr1 kmax omax 127

    let df2' = zzdf $ funcsTreesMap (\(ss,ff) -> (ss, (ff `funion` gg1) `fdep` fder ff)) $ dfzz df2

    BL.writeFile (model ++ ".json") $ decompFudsPersistentsEncode $ decompFudsPersistent df2'
    printf "<<< done %s\n" $ model
    hFlush stdout

    printf "decomp size: %d\n" $ (Set.size $ fvars $ dfff df2')
    hFlush stdout

    printf "<<< done\n"


