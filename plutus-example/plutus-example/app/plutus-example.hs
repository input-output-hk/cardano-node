{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}


import           Prelude

import           Cardano.Api

import           Data.Maybe
import           System.Directory
import           System.FilePath.Posix ((</>))

import           Cardano.Ledger.Alonzo.Scripts as Alonzo
import           Cardano.Ledger.Alonzo.Tools
import           Cardano.Ledger.Alonzo.TxInfo
import           Cardano.PlutusExample.AlwaysSucceeds (alwaysSucceedsScript,
                   alwaysSucceedsScriptShortBs)
import           Cardano.PlutusExample.CustomDatumRedeemerGuess
import           Cardano.PlutusExample.DatumRedeemerGuess (guessScript)
import           Cardano.PlutusExample.MintingScript (apiExamplePlutusMintingScript)
import           Plutus.V1.Ledger.Api
import           PlutusCore.Evaluation.Machine.ExBudget

main :: IO ()
main = do
  let dir = "generated-plutus-scripts"
  createDirectory dir

  print @String ("Counting" )
  print $ estimateCounting alwaysSucceedsScriptShortBs
  estimateRestricting alwaysSucceedsScriptShortBs

  _ <- writeFileTextEnvelope (dir </> "always-succeeds-spending.plutus") Nothing alwaysSucceedsScript
  _ <- writeFileTextEnvelope (dir </> "guess-42-datum-42-txin.plutus") Nothing guessScript
  _ <- writeFileTextEnvelope (dir </> "custom-guess-42-datum-42.plutus") Nothing customGuessScript
  _ <- writeFileTextEnvelope (dir </> "anyone-can-mint.plutus") Nothing apiExamplePlutusMintingScript
  return ()

estimateCounting :: SerializedScript -> (LogOutput, Either EvaluationError ExBudget)
estimateCounting sScript = do
  let costModel = fromMaybe (error "corrupt default cost model") defaultCostModelParams
  evaluateScriptCounting Quiet costModel sScript [I 42, I 42]


estimateRestricting sScript = do
  let (_, eBudget) = estimateCounting sScript
  case eBudget of
    Left err -> error $ show err
    Right bud -> do
      let costModel = fromMaybe (error "corrupt default cost model") defaultCostModelParams
      print @String ("Restricting" )
      print $ evaluateScriptRestricting Quiet costModel bud sScript [I 42, I 42]
      case exBudgetToExUnits bud of
        Just exunits -> print $ runPLCScript (Alonzo.CostModel costModel) sScript exunits [I 42, I 42]
        Nothing -> error "Failed to convert units"

exBudgetToExUnits :: ExBudget -> Maybe ExUnits
exBudgetToExUnits (ExBudget (ExCPU steps) (ExMemory memory)) =
  ExUnits <$> safeFromInteger (toInteger memory)
    <*> safeFromInteger (toInteger steps)
  where
    safeFromInteger :: forall a. (Integral a, Bounded a) => Integer -> Maybe a
    safeFromInteger i
      | toInteger (minBound :: a) <= i && i <= toInteger (maxBound :: a) = Just $ fromInteger i
      | otherwise = Nothing
