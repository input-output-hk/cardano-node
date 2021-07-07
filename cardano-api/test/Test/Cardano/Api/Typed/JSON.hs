{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Api.Typed.JSON
  ( tests
  ) where

import           Cardano.Prelude

import           Data.Aeson (eitherDecode, encode)
import           Gen.Tasty.Hedgehog.Group (fromGroup)
import           Hedgehog (Property, discover, forAll, tripping)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import           Test.Cardano.Api.Typed.Orphans ()
import           Test.Tasty (TestTree)

import           Cardano.Api

import           Gen.Cardano.Api.Typed

{- HLINT ignore "Use camelCase" -}

prop_roundtrip_praos_nonce_JSON :: Property
prop_roundtrip_praos_nonce_JSON = H.property $ do
  pNonce <- forAll $ Gen.just genMaybePraosNonce
  tripping pNonce encode eitherDecode

roundtrip_protocol_parameters_JSON :: CardanoEra era -> Property
roundtrip_protocol_parameters_JSON era = H.property $ do
  pp <- forAll $ genProtocolParameters era
  tripping pp encode eitherDecode

prop_roundtrip_protocol_parameters_JSON_Byron :: Property
prop_roundtrip_protocol_parameters_JSON_Byron =
  roundtrip_protocol_parameters_JSON ByronEra

prop_roundtrip_protocol_parameters_JSON_Shelley :: Property
prop_roundtrip_protocol_parameters_JSON_Shelley =
  roundtrip_protocol_parameters_JSON ShelleyEra

prop_roundtrip_protocol_parameters_JSON_Allegra :: Property
prop_roundtrip_protocol_parameters_JSON_Allegra =
  roundtrip_protocol_parameters_JSON AllegraEra

prop_roundtrip_protocol_parameters_JSON_Mary :: Property
prop_roundtrip_protocol_parameters_JSON_Mary =
  roundtrip_protocol_parameters_JSON MaryEra

prop_roundtrip_protocol_parameters_JSON_Alonzo :: Property
prop_roundtrip_protocol_parameters_JSON_Alonzo =
  roundtrip_protocol_parameters_JSON AlonzoEra

-- -----------------------------------------------------------------------------

tests :: TestTree
tests = fromGroup $$discover
