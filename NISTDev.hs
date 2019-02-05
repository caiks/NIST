{-# LANGUAGE DeriveGeneric, OverloadedStrings, BangPatterns #-}

module NISTDev
where

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

data IntOrNull = IntOrNullInt Int | IntOrNullNull deriving Show

instance FromField IntOrNull where
  parseField s = case runParser (parseField s :: Parser Int) of
    Left err -> pure $ IntOrNullNull
    Right n  -> pure $ IntOrNullInt n

instance ToValue IntOrNull where
  toValue IntOrNullNull = ValStr "null"
  toValue (IntOrNullInt i) = toValue i

data DoubleOrNull = DoubleOrNullDouble Double | DoubleOrNullNull deriving Show

instance FromField DoubleOrNull where
  parseField s = case runParser (parseField s :: Parser Double) of
    Left err -> pure $ DoubleOrNullNull
    Right n  -> pure $ DoubleOrNullDouble n

instance ToValue DoubleOrNull where
  toValue DoubleOrNullNull = ValStr "null"
  toValue (DoubleOrNullDouble d) = toValue d

data Train = Train { 
  trId :: !IntOrNull,
  trMSSubClass :: !IntOrNull,
  trMSZoning :: !String,
  trLotFrontage :: !IntOrNull,
  trLotArea :: !IntOrNull,
  trStreet :: !String,
  trAlley :: !String,
  trLotShape :: !String,
  trLandContour :: !String,
  trUtilities :: !String,
  trLotConfig :: !String,
  trLandSlope :: !String,
  trNeighborhood :: !String,
  trCondition1 :: !String,
  trCondition2 :: !String,
  trBldgType :: !String,
  trHouseStyle :: !String,
  trOverallQual :: !IntOrNull,
  trOverallCond :: !IntOrNull,
  trYearBuilt :: !IntOrNull,
  trYearRemodAdd :: !IntOrNull,
  trRoofStyle :: !String,
  trRoofMatl :: !String,
  trExterior1st :: !String,
  trExterior2nd :: !String,
  trMasVnrType :: !String,
  trMasVnrArea :: !IntOrNull,
  trExterQual :: !String,
  trExterCond :: !String,
  trFoundation :: !String,
  trBsmtQual :: !String,
  trBsmtCond :: !String,
  trBsmtExposure :: !String,
  trBsmtFinType1 :: !String,
  trBsmtFinSF1 :: !IntOrNull,
  trBsmtFinType2 :: !String,
  trBsmtFinSF2 :: !IntOrNull,
  trBsmtUnfSF :: !IntOrNull,
  trTotalBsmtSF :: !IntOrNull,
  trHeating :: !String,
  trHeatingQC :: !String,
  trCentralAir :: !String,
  trElectrical :: !String,
  tr1stFlrSF :: !IntOrNull,
  tr2ndFlrSF :: !IntOrNull,
  trLowQualFinSF :: !IntOrNull,
  trGrLivArea :: !IntOrNull,
  trBsmtFullBath :: !IntOrNull,
  trBsmtHalfBath :: !IntOrNull,
  trFullBath :: !IntOrNull,
  trHalfBath :: !IntOrNull,
  trBedroomAbvGr :: !IntOrNull,
  trKitchenAbvGr :: !IntOrNull,
  trKitchenQual :: !String,
  trTotRmsAbvGrd :: !IntOrNull,
  trFunctional :: !String,
  trFireplaces :: !IntOrNull,
  trFireplaceQu :: !String,
  trGarageType :: !String,
  trGarageYrBlt :: !IntOrNull,
  trGarageFinish :: !String,
  trGarageCars :: !IntOrNull,
  trGarageArea :: !IntOrNull,
  trGarageQual :: !String,
  trGarageCond :: !String,
  trPavedDrive :: !String,
  trWoodDeckSF :: !IntOrNull,
  trOpenPorchSF :: !IntOrNull,
  trEnclosedPorch :: !IntOrNull,
  tr3SsnPorch :: !IntOrNull,
  trScreenPorch :: !IntOrNull,
  trPoolArea :: !IntOrNull,
  trPoolQC :: !String,
  trFence :: !String,
  trMiscFeature :: !String,
  trMiscVal :: !IntOrNull,
  trMoSold :: !IntOrNull,
  trYrSold :: !IntOrNull,
  trSaleType :: !String,
  trSaleCondition :: !String,
  trSalePrice :: !IntOrNull} 
  deriving (Generic, Show)

instance FromRecord Train

trmap = [
  ("Id", toValue . trId),
  ("MSSubClass", toValue . trMSSubClass),
  ("MSZoning", toValue . trMSZoning),
  ("LotFrontage", toValue . trLotFrontage),
  ("LotArea", toValue . trLotArea),
  ("Street", toValue . trStreet),
  ("Alley", toValue . trAlley),
  ("LotShape", toValue . trLotShape),
  ("LandContour", toValue . trLandContour),
  ("Utilities", toValue . trUtilities),
  ("LotConfig", toValue . trLotConfig),
  ("LandSlope", toValue . trLandSlope),
  ("Neighborhood", toValue . trNeighborhood),
  ("Condition1", toValue . trCondition1),
  ("Condition2", toValue . trCondition2),
  ("BldgType", toValue . trBldgType),
  ("HouseStyle", toValue . trHouseStyle),
  ("OverallQual", toValue . trOverallQual),
  ("OverallCond", toValue . trOverallCond),
  ("YearBuilt", toValue . trYearBuilt),
  ("YearRemodAdd", toValue . trYearRemodAdd),
  ("RoofStyle", toValue . trRoofStyle),
  ("RoofMatl", toValue . trRoofMatl),
  ("Exterior1st", toValue . trExterior1st),
  ("Exterior2nd", toValue . trExterior2nd),
  ("MasVnrType", toValue . trMasVnrType),
  ("MasVnrArea", toValue . trMasVnrArea),
  ("ExterQual", toValue . trExterQual),
  ("ExterCond", toValue . trExterCond),
  ("Foundation", toValue . trFoundation),
  ("BsmtQual", toValue . trBsmtQual),
  ("BsmtCond", toValue . trBsmtCond),
  ("BsmtExposure", toValue . trBsmtExposure),
  ("BsmtFinType1", toValue . trBsmtFinType1),
  ("BsmtFinSF1", toValue . trBsmtFinSF1),
  ("BsmtFinType2", toValue . trBsmtFinType2),
  ("BsmtFinSF2", toValue . trBsmtFinSF2),
  ("BsmtUnfSF", toValue . trBsmtUnfSF),
  ("TotalBsmtSF", toValue . trTotalBsmtSF),
  ("Heating", toValue . trHeating),
  ("HeatingQC", toValue . trHeatingQC),
  ("CentralAir", toValue . trCentralAir),
  ("Electrical", toValue . trElectrical),
  ("1stFlrSF", toValue . tr1stFlrSF),
  ("2ndFlrSF", toValue . tr2ndFlrSF),
  ("LowQualFinSF", toValue . trLowQualFinSF),
  ("GrLivArea", toValue . trGrLivArea),
  ("BsmtFullBath", toValue . trBsmtFullBath),
  ("BsmtHalfBath", toValue . trBsmtHalfBath),
  ("FullBath", toValue . trFullBath),
  ("HalfBath", toValue . trHalfBath),
  ("BedroomAbvGr", toValue . trBedroomAbvGr),
  ("KitchenAbvGr", toValue . trKitchenAbvGr),
  ("KitchenQual", toValue . trKitchenQual),
  ("TotRmsAbvGrd", toValue . trTotRmsAbvGrd),
  ("Functional", toValue . trFunctional),
  ("Fireplaces", toValue . trFireplaces),
  ("FireplaceQu", toValue . trFireplaceQu),
  ("GarageType", toValue . trGarageType),
  ("GarageYrBlt", toValue . trGarageYrBlt),
  ("GarageFinish", toValue . trGarageFinish),
  ("GarageCars", toValue . trGarageCars),
  ("GarageArea", toValue . trGarageArea),
  ("GarageQual", toValue . trGarageQual),
  ("GarageCond", toValue . trGarageCond),
  ("PavedDrive", toValue . trPavedDrive),
  ("WoodDeckSF", toValue . trWoodDeckSF),
  ("OpenPorchSF", toValue . trOpenPorchSF),
  ("EnclosedPorch", toValue . trEnclosedPorch),
  ("3SsnPorch", toValue . tr3SsnPorch),
  ("ScreenPorch", toValue . trScreenPorch),
  ("PoolArea", toValue . trPoolArea),
  ("PoolQC", toValue . trPoolQC),
  ("Fence", toValue . trFence),
  ("MiscFeature", toValue . trMiscFeature),
  ("MiscVal", toValue . trMiscVal),
  ("MoSold", toValue . trMoSold),
  ("YrSold", toValue . trYrSold),
  ("SaleType", toValue . trSaleType),
  ("SaleCondition", toValue . trSaleCondition),
  ("SalePrice", toValue . trSalePrice)]

data Test = Test { 
  teId :: !IntOrNull,
  teMSSubClass :: !IntOrNull,
  teMSZoning :: !String,
  teLotFrontage :: !IntOrNull,
  teLotArea :: !IntOrNull,
  teStreet :: !String,
  teAlley :: !String,
  teLotShape :: !String,
  teLandContour :: !String,
  teUtilities :: !String,
  teLotConfig :: !String,
  teLandSlope :: !String,
  teNeighborhood :: !String,
  teCondition1 :: !String,
  teCondition2 :: !String,
  teBldgType :: !String,
  teHouseStyle :: !String,
  teOverallQual :: !IntOrNull,
  teOverallCond :: !IntOrNull,
  teYearBuilt :: !IntOrNull,
  teYearRemodAdd :: !IntOrNull,
  teRoofStyle :: !String,
  teRoofMatl :: !String,
  teExterior1st :: !String,
  teExterior2nd :: !String,
  teMasVnrType :: !String,
  teMasVnrArea :: !IntOrNull,
  teExterQual :: !String,
  teExterCond :: !String,
  teFoundation :: !String,
  teBsmtQual :: !String,
  teBsmtCond :: !String,
  teBsmtExposure :: !String,
  teBsmtFinType1 :: !String,
  teBsmtFinSF1 :: !IntOrNull,
  teBsmtFinType2 :: !String,
  teBsmtFinSF2 :: !IntOrNull,
  teBsmtUnfSF :: !IntOrNull,
  teTotalBsmtSF :: !IntOrNull,
  teHeating :: !String,
  teHeatingQC :: !String,
  teCentralAir :: !String,
  teElectrical :: !String,
  te1stFlrSF :: !IntOrNull,
  te2ndFlrSF :: !IntOrNull,
  teLowQualFinSF :: !IntOrNull,
  teGrLivArea :: !IntOrNull,
  teBsmtFullBath :: !IntOrNull,
  teBsmtHalfBath :: !IntOrNull,
  teFullBath :: !IntOrNull,
  teHalfBath :: !IntOrNull,
  teBedroomAbvGr :: !IntOrNull,
  teKitchenAbvGr :: !IntOrNull,
  teKitchenQual :: !String,
  teTotRmsAbvGrd :: !IntOrNull,
  teFunctional :: !String,
  teFireplaces :: !IntOrNull,
  teFireplaceQu :: !String,
  teGarageType :: !String,
  teGarageYrBlt :: !IntOrNull,
  teGarageFinish :: !String,
  teGarageCars :: !IntOrNull,
  teGarageArea :: !IntOrNull,
  teGarageQual :: !String,
  teGarageCond :: !String,
  tePavedDrive :: !String,
  teWoodDeckSF :: !IntOrNull,
  teOpenPorchSF :: !IntOrNull,
  teEnclosedPorch :: !IntOrNull,
  te3SsnPorch :: !IntOrNull,
  teScreenPorch :: !IntOrNull,
  tePoolArea :: !IntOrNull,
  tePoolQC :: !String,
  teFence :: !String,
  teMiscFeature :: !String,
  teMiscVal :: !IntOrNull,
  teMoSold :: !IntOrNull,
  teYrSold :: !IntOrNull,
  teSaleType :: !String,
  teSaleCondition :: !String} 
  deriving (Generic, Show)

instance FromRecord Test

temap = [
  ("Id", toValue . teId),
  ("MSSubClass", toValue . teMSSubClass),
  ("MSZoning", toValue . teMSZoning),
  ("LotFrontage", toValue . teLotFrontage),
  ("LotArea", toValue . teLotArea),
  ("Street", toValue . teStreet),
  ("Alley", toValue . teAlley),
  ("LotShape", toValue . teLotShape),
  ("LandContour", toValue . teLandContour),
  ("Utilities", toValue . teUtilities),
  ("LotConfig", toValue . teLotConfig),
  ("LandSlope", toValue . teLandSlope),
  ("Neighborhood", toValue . teNeighborhood),
  ("Condition1", toValue . teCondition1),
  ("Condition2", toValue . teCondition2),
  ("BldgType", toValue . teBldgType),
  ("HouseStyle", toValue . teHouseStyle),
  ("OverallQual", toValue . teOverallQual),
  ("OverallCond", toValue . teOverallCond),
  ("YearBuilt", toValue . teYearBuilt),
  ("YearRemodAdd", toValue . teYearRemodAdd),
  ("RoofStyle", toValue . teRoofStyle),
  ("RoofMatl", toValue . teRoofMatl),
  ("Exterior1st", toValue . teExterior1st),
  ("Exterior2nd", toValue . teExterior2nd),
  ("MasVnrType", toValue . teMasVnrType),
  ("MasVnrArea", toValue . teMasVnrArea),
  ("ExterQual", toValue . teExterQual),
  ("ExterCond", toValue . teExterCond),
  ("Foundation", toValue . teFoundation),
  ("BsmtQual", toValue . teBsmtQual),
  ("BsmtCond", toValue . teBsmtCond),
  ("BsmtExposure", toValue . teBsmtExposure),
  ("BsmtFinType1", toValue . teBsmtFinType1),
  ("BsmtFinSF1", toValue . teBsmtFinSF1),
  ("BsmtFinType2", toValue . teBsmtFinType2),
  ("BsmtFinSF2", toValue . teBsmtFinSF2),
  ("BsmtUnfSF", toValue . teBsmtUnfSF),
  ("TotalBsmtSF", toValue . teTotalBsmtSF),
  ("Heating", toValue . teHeating),
  ("HeatingQC", toValue . teHeatingQC),
  ("CentralAir", toValue . teCentralAir),
  ("Electrical", toValue . teElectrical),
  ("1stFlrSF", toValue . te1stFlrSF),
  ("2ndFlrSF", toValue . te2ndFlrSF),
  ("LowQualFinSF", toValue . teLowQualFinSF),
  ("GrLivArea", toValue . teGrLivArea),
  ("BsmtFullBath", toValue . teBsmtFullBath),
  ("BsmtHalfBath", toValue . teBsmtHalfBath),
  ("FullBath", toValue . teFullBath),
  ("HalfBath", toValue . teHalfBath),
  ("BedroomAbvGr", toValue . teBedroomAbvGr),
  ("KitchenAbvGr", toValue . teKitchenAbvGr),
  ("KitchenQual", toValue . teKitchenQual),
  ("TotRmsAbvGrd", toValue . teTotRmsAbvGrd),
  ("Functional", toValue . teFunctional),
  ("Fireplaces", toValue . teFireplaces),
  ("FireplaceQu", toValue . teFireplaceQu),
  ("GarageType", toValue . teGarageType),
  ("GarageYrBlt", toValue . teGarageYrBlt),
  ("GarageFinish", toValue . teGarageFinish),
  ("GarageCars", toValue . teGarageCars),
  ("GarageArea", toValue . teGarageArea),
  ("GarageQual", toValue . teGarageQual),
  ("GarageCond", toValue . teGarageCond),
  ("PavedDrive", toValue . tePavedDrive),
  ("WoodDeckSF", toValue . teWoodDeckSF),
  ("OpenPorchSF", toValue . teOpenPorchSF),
  ("EnclosedPorch", toValue . teEnclosedPorch),
  ("3SsnPorch", toValue . te3SsnPorch),
  ("ScreenPorch", toValue . teScreenPorch),
  ("PoolArea", toValue . tePoolArea),
  ("PoolQC", toValue . tePoolQC),
  ("Fence", toValue . teFence),
  ("MiscFeature", toValue . teMiscFeature),
  ("MiscVal", toValue . teMiscVal),
  ("MoSold", toValue . teMoSold),
  ("YrSold", toValue . teYrSold),
  ("SaleType", toValue . teSaleType),
  ("SaleCondition", toValue . teSaleCondition)]

isOrd uu v = or (Set.map isOrdVal (fromJust (systemsVarsValues uu v)))
  where
    isOrdVal (ValInt _) = True
    isOrdVal (ValDouble _) = True
    isOrdVal _ = False

bucket b aa v = 
    Map.fromList $ reverse $ concat [zip jj (repeat (last jj)) | jj <- slice (length ll `div` b) ll]
  where 
    ll = sort $ map (snd . head . ssll) $ snd $ unzip $ hhll $ aahh (aa `red` sgl v)
    slice k ll 
      | length ll < k = [ll] 
      | otherwise = take k ll : slice k (drop k ll)

reframeb aa xx =
    llaa $ map (\(ss,q) -> (repl ss xx, q)) $ aall aa
  where
    repl ss xx = llss $ [(w, ww `findu` u) | (v,u) <- ssll ss, let (w,ww) = xx `findv` v]
    findu mm x = Map.findWithDefault x x mm
    findv mm x = Map.findWithDefault (x,Map.empty) x mm

aahr uu aa = hhhr uu $ aahh aa 

decomperIO uu vv hh wmax lmax xmax omax bmax mmax umax pmax fmax mult seed =
      parametersSystemsHistoryRepasDecomperMaxRollByMExcludedSelfHighestFmaxIORepa 
        wmax lmax xmax omax bmax mmax umax pmax fmax mult seed uu vv hh



