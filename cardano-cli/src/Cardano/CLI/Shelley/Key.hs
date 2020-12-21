{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Shelley CLI option data types and functions for cryptographic keys.
module Cardano.CLI.Shelley.Key
  ( OutputDirection (..)
  , writeOutputBech32

  , readSigningKeyFile
  , readSigningKeyFileAnyOf

  , VerificationKeyOrFile (..)
  , readVerificationKeyOrFile
  , readVerificationKeyOrTextEnvFile

  , VerificationKeyTextOrFile (..)
  , VerificationKeyTextOrFileError (..)
  , readVerificationKeyTextOrFileAnyOf
  , renderVerificationKeyTextOrFileError

  , VerificationKeyOrHashOrFile (..)
  , readVerificationKeyOrHashOrFile
  , readVerificationKeyOrHashOrTextEnvFile
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except.Extra (handleIOExceptT)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Cardano.Api
import           Cardano.Api.DeserialiseAnyOf

import           Cardano.CLI.Types

------------------------------------------------------------------------------
-- Formatted/encoded output serialisation
------------------------------------------------------------------------------

-- | Where to write some output.
data OutputDirection
  = OutputDirectionStdout
  -- ^ Write output to @stdout@.
  | OutputDirectionFile !FilePath
  -- ^ Write output to a provided file.
  deriving (Eq, Show)

writeOutputBech32
  :: SerialiseAsBech32 a
  => OutputDirection
  -> a
  -> IO (Either (FileError ()) ())
writeOutputBech32 outputDirection a =
    case outputDirection of
      OutputDirectionStdout -> Right <$> BSC.putStrLn outputBs
      OutputDirectionFile fp ->
        runExceptT $ handleIOExceptT (FileIOError fp) $
          BS.writeFile fp outputBs
  where
    outputBs :: ByteString
    outputBs = Text.encodeUtf8 (serialiseToBech32 a)

------------------------------------------------------------------------------
-- Signing key deserialisation
------------------------------------------------------------------------------

-- | Read a signing key from a file.
--
-- The contents of the file can either be Bech32-encoded, hex-encoded, or in
-- the text envelope format.
readSigningKeyFile
  :: forall keyrole.
     ( HasTextEnvelope (SigningKey keyrole)
     , SerialiseAsBech32 (SigningKey keyrole)
     )
  => AsType keyrole
  -> SigningKeyFile
  -> IO (Either (FileError InputDecodeError) (SigningKey keyrole))
readSigningKeyFile asType (SigningKeyFile fp) =
  readFileAnyOfInputFormats
    (AsSigningKey asType)
    [InputFormatBech32, InputFormatHex, InputFormatTextEnvelope]
    fp

-- | Read a signing key from a file given that it is one of the provided types
-- of signing key.
--
-- The contents of the file can either be Bech32-encoded or in the text
-- envelope format.
readSigningKeyFileAnyOf
  :: forall b.
     [FromSomeType SerialiseAsBech32 b]
  -> [FromSomeType HasTextEnvelope b]
  -> SigningKeyFile
  -> IO (Either (FileError InputDecodeError) b)
readSigningKeyFileAnyOf bech32Types textEnvTypes (SigningKeyFile fp) =
  readFileBech32OrTextEnvAnyOf bech32Types textEnvTypes fp

------------------------------------------------------------------------------
-- Verification key deserialisation
------------------------------------------------------------------------------

-- | Either a verification key or path to a verification key file.
data VerificationKeyOrFile keyrole
  = VerificationKeyValue !(VerificationKey keyrole)
  -- ^ A verification key.
  | VerificationKeyFilePath !VerificationKeyFile
  -- ^ A path to a verification key file.
  -- Note that this file hasn't been validated at all (whether it exists,
  -- contains a key of the correct type, etc.)

deriving instance Show (VerificationKey keyrole)
  => Show (VerificationKeyOrFile keyrole)

deriving instance Eq (VerificationKey keyrole)
  => Eq (VerificationKeyOrFile keyrole)

-- | Read a verification key or verification key file and return a
-- verification key.
--
-- If a filepath is provided, the file can either be formatted as Bech32, hex,
-- or text envelope.
readVerificationKeyOrFile
  :: ( HasTextEnvelope (VerificationKey keyrole)
     , SerialiseAsBech32 (VerificationKey keyrole)
     )
  => AsType keyrole
  -> VerificationKeyOrFile keyrole
  -> IO (Either (FileError InputDecodeError) (VerificationKey keyrole))
readVerificationKeyOrFile asType verKeyOrFile =
  case verKeyOrFile of
    VerificationKeyValue vk -> pure (Right vk)
    VerificationKeyFilePath (VerificationKeyFile fp) ->
      readFileAnyOfInputFormats
        (AsVerificationKey asType)
        [InputFormatBech32, InputFormatHex, InputFormatTextEnvelope]
        fp

-- | Read a verification key or verification key file and return a
-- verification key.
--
-- If a filepath is provided, it will be interpreted as a text envelope
-- formatted file.
readVerificationKeyOrTextEnvFile
  :: HasTextEnvelope (VerificationKey keyrole)
  => AsType keyrole
  -> VerificationKeyOrFile keyrole
  -> IO (Either (FileError InputDecodeError) (VerificationKey keyrole))
readVerificationKeyOrTextEnvFile asType verKeyOrFile =
  case verKeyOrFile of
    VerificationKeyValue vk -> pure (Right vk)
    VerificationKeyFilePath (VerificationKeyFile fp) ->
      readFileTextEnvelope' (AsVerificationKey asType) fp

-- | Either an unvalidated text representation of a verification key or a path
-- to a verification key file.
data VerificationKeyTextOrFile
  = VktofVerificationKeyText !Text
  | VktofVerificationKeyFile !VerificationKeyFile
  deriving (Eq, Show)

-- | An error in deserializing a 'VerificationKeyTextOrFile' to a
-- 'VerificationKey'.
data VerificationKeyTextOrFileError
  = VerificationKeyTextError !InputDecodeError
  | VerificationKeyFileError !(FileError InputDecodeError)
  deriving Show

-- | Render an error message for a 'VerificationKeyTextOrFileError'.
renderVerificationKeyTextOrFileError :: VerificationKeyTextOrFileError -> Text
renderVerificationKeyTextOrFileError vkTextOrFileErr =
  case vkTextOrFileErr of
    VerificationKeyTextError err -> renderInputDecodeError err
    VerificationKeyFileError err -> Text.pack (displayError err)

-- | Deserialise a verification key from text or a verification key file given
-- that it is one of the provided types.
--
-- If a filepath is provided, the file can either be formatted as Bech32, hex,
-- or text envelope.
readVerificationKeyTextOrFileAnyOf
  :: forall b.
     [FromSomeType SerialiseAsBech32 b]
  -> [FromSomeType HasTextEnvelope b]
  -> VerificationKeyTextOrFile
  -> IO (Either VerificationKeyTextOrFileError b)
readVerificationKeyTextOrFileAnyOf bech32Types textEnvTypes verKeyTextOrFile =
  case verKeyTextOrFile of
    VktofVerificationKeyText vkText ->
      pure $ first VerificationKeyTextError $
        deserialiseInputAnyOf bech32Types textEnvTypes (Text.encodeUtf8 vkText)
    VktofVerificationKeyFile (VerificationKeyFile fp) ->
      first VerificationKeyFileError
        <$> readFileBech32OrTextEnvAnyOf bech32Types textEnvTypes fp

-- | Verification key, verification key hash, or path to a verification key
-- file.
data VerificationKeyOrHashOrFile keyrole
  = VerificationKeyOrFile !(VerificationKeyOrFile keyrole)
  -- ^ Either a verification key or path to a verification key file.
  | VerificationKeyHash !(Hash keyrole)
  -- ^ A verification key hash.

deriving instance (Show (VerificationKeyOrFile keyrole), Show (Hash keyrole))
  => Show (VerificationKeyOrHashOrFile keyrole)

deriving instance (Eq (VerificationKeyOrFile keyrole), Eq (Hash keyrole))
  => Eq (VerificationKeyOrHashOrFile keyrole)

-- | Read a verification key or verification key hash or verification key file
-- and return a verification key hash.
--
-- If a filepath is provided, the file can either be formatted as Bech32, hex,
-- or text envelope.
readVerificationKeyOrHashOrFile
  :: (Key keyrole, SerialiseAsBech32 (VerificationKey keyrole))
  => AsType keyrole
  -> VerificationKeyOrHashOrFile keyrole
  -> IO (Either (FileError InputDecodeError) (Hash keyrole))
readVerificationKeyOrHashOrFile asType verKeyOrHashOrFile =
  case verKeyOrHashOrFile of
    VerificationKeyOrFile vkOrFile -> do
      eitherVk <- readVerificationKeyOrFile asType vkOrFile
      pure (verificationKeyHash <$> eitherVk)
    VerificationKeyHash vkHash -> pure (Right vkHash)

-- | Read a verification key or verification key hash or verification key file
-- and return a verification key hash.
--
-- If a filepath is provided, it will be interpreted as a text envelope
-- formatted file.
readVerificationKeyOrHashOrTextEnvFile
  :: Key keyrole
  => AsType keyrole
  -> VerificationKeyOrHashOrFile keyrole
  -> IO (Either (FileError InputDecodeError) (Hash keyrole))
readVerificationKeyOrHashOrTextEnvFile asType verKeyOrHashOrFile =
  case verKeyOrHashOrFile of
    VerificationKeyOrFile vkOrFile -> do
      eitherVk <- readVerificationKeyOrTextEnvFile asType vkOrFile
      pure (verificationKeyHash <$> eitherVk)
    VerificationKeyHash vkHash -> pure (Right vkHash)
