{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Deserialisation of input that may be formatted/encoded in one of several specified
-- ways.
module Cardano.Api.DeserialiseAnyOf
  ( InputFormat (..)
  , InputDecodeError (..)
  , deserialiseInput
  , deserialiseInputAnyOf
  , renderInputDecodeError

  , readFileAnyOfInputFormats
  , readFileBech32OrTextEnvAnyOf
  , readFileTextEnvelope'
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Cardano.Api.Typed

------------------------------------------------------------------------------
-- Formatted/encoded input deserialisation
------------------------------------------------------------------------------

-- | Input format/encoding.
data InputFormat a where
  -- | Bech32 encoding.
  InputFormatBech32 :: SerialiseAsBech32 a => InputFormat a

  -- | Hex/Base16 encoding.
  InputFormatHex :: SerialiseAsRawBytes a => InputFormat a

  -- | Text envelope format.
  InputFormatTextEnvelope :: HasTextEnvelope a => InputFormat a

-- | Input decoding error.
data InputDecodeError
  = InputTextEnvelopeError !TextEnvelopeError
  -- ^ The provided data seems to be a valid text envelope, but some error
  -- occurred in deserialising it.
  | InputBech32DecodeError !Bech32DecodeError
  -- ^ The provided data is valid Bech32, but some error occurred in
  -- deserialising it.
  | InputHexDecodeError
  -- ^ The provided data is valid hex, but some error occurred in
  -- deserialising it.
  | InputInvalidFormatError
  -- ^ The provided data is not formatted in one of the expected formats.
  --
  -- TODO: Add parameter for list of possible input formats.
  deriving (Eq, Show)

instance Error InputDecodeError where
  displayError = Text.unpack . renderInputDecodeError

-- | Render an error message for a 'InputDecodeError'.
renderInputDecodeError :: InputDecodeError -> Text
renderInputDecodeError err =
  case err of
    InputTextEnvelopeError textEnvErr ->
      Text.pack (displayError textEnvErr)
    InputBech32DecodeError decodeErr ->
      Text.pack (displayError decodeErr)
    InputHexDecodeError ->
      "There was an error in deserialising the hex-encoded string into a "
        <> "value of the expected type."
    InputInvalidFormatError ->
      "The provided data is not formatted in one of the expected formats."

-- | The result of a deserialisation function.
--
-- Note that this type isn't intended to be exported, but only used as a
-- helper within the 'deserialiseInput' function.
data DeserialiseInputResult a
  = DeserialiseInputSuccess !a
  -- ^ Input successfully deserialised.
  | DeserialiseInputError !InputDecodeError
  -- ^ The provided data is of the expected format/encoding, but an error
  -- occurred in deserializing it.
  | DeserialiseInputErrorFormatMismatch
  -- ^ The provided data's formatting/encoding does not match that which was
  -- expected. This error is an indication that one could attempt to
  -- deserialise the input again, but instead expecting a different format.

-- | Deserialise an input of some type that is formatted in some way.
deserialiseInput
  :: forall a.
     AsType a
  -> [InputFormat a]
  -> ByteString
  -> Either InputDecodeError a
deserialiseInput asType acceptedFormats inputBs =
    go acceptedFormats
  where
    inputText :: Text
    inputText = Text.decodeUtf8 inputBs

    go :: [InputFormat a] -> Either InputDecodeError a
    go [] = Left InputInvalidFormatError
    go (kf:kfs) =
      let res =
            case kf of
              InputFormatBech32 -> deserialiseBech32
              InputFormatHex -> deserialiseHex
              InputFormatTextEnvelope -> deserialiseTextEnvelope
      in case res of
        DeserialiseInputSuccess a -> Right a
        DeserialiseInputError err -> Left err
        DeserialiseInputErrorFormatMismatch -> go kfs

    deserialiseTextEnvelope :: HasTextEnvelope a => DeserialiseInputResult a
    deserialiseTextEnvelope = do
      let textEnvRes :: Either TextEnvelopeError a
          textEnvRes =
            deserialiseFromTextEnvelope asType
              =<< first TextEnvelopeAesonDecodeError (Aeson.eitherDecodeStrict' inputBs)
      case textEnvRes of
        Right res -> DeserialiseInputSuccess res

        -- The input was valid a text envelope, but there was a type mismatch
        -- error.
        Left err@TextEnvelopeTypeError{} ->
          DeserialiseInputError (InputTextEnvelopeError err)

        -- The input was not valid a text envelope.
        Left _ -> DeserialiseInputErrorFormatMismatch

    deserialiseBech32 :: SerialiseAsBech32 a => DeserialiseInputResult a
    deserialiseBech32 =
      case deserialiseFromBech32 asType inputText of
        Right res -> DeserialiseInputSuccess res

        -- The input was not valid Bech32.
        Left (Bech32DecodingError _) -> DeserialiseInputErrorFormatMismatch

        -- The input was valid Bech32, but some other error occurred.
        Left err -> DeserialiseInputError $ InputBech32DecodeError err

    deserialiseHex :: SerialiseAsRawBytes a => DeserialiseInputResult a
    deserialiseHex
      | isValidHex inputBs =
          maybe
            (DeserialiseInputError InputHexDecodeError)
            DeserialiseInputSuccess
            (deserialiseFromRawBytesHex asType inputBs)
      | otherwise = DeserialiseInputErrorFormatMismatch

    isValidHex :: ByteString -> Bool
    isValidHex x =
      all (`elem` hexAlpha) (toLower <$> BSC.unpack x)
        && even (BSC.length x)
      where
        hexAlpha :: [Char]
        hexAlpha = "0123456789abcdef"

-- | Deserialise an input of some type that is formatted in some way.
--
-- The provided 'ByteString' can either be Bech32-encoded or in the text
-- envelope format.
deserialiseInputAnyOf
  :: forall b.
     [FromSomeType SerialiseAsBech32 b]
  -> [FromSomeType HasTextEnvelope b]
  -> ByteString
  -> Either InputDecodeError b
deserialiseInputAnyOf bech32Types textEnvTypes inputBs =
    case deserialiseBech32 `orTry` deserialiseTextEnvelope of
      DeserialiseInputSuccess res -> Right res
      DeserialiseInputError err -> Left err
      DeserialiseInputErrorFormatMismatch -> Left InputInvalidFormatError
  where
    inputText :: Text
    inputText = Text.decodeUtf8 inputBs

    orTry
      :: DeserialiseInputResult b
      -> DeserialiseInputResult b
      -> DeserialiseInputResult b
    orTry x y =
      case x of
        DeserialiseInputSuccess _ -> x
        DeserialiseInputError _ -> x
        DeserialiseInputErrorFormatMismatch -> y

    deserialiseTextEnvelope :: DeserialiseInputResult b
    deserialiseTextEnvelope = do
      let textEnvRes :: Either TextEnvelopeError b
          textEnvRes =
            deserialiseFromTextEnvelopeAnyOf textEnvTypes
              =<< first TextEnvelopeAesonDecodeError (Aeson.eitherDecodeStrict' inputBs)
      case textEnvRes of
        Right res -> DeserialiseInputSuccess res

        -- The input was valid a text envelope, but there was a type mismatch
        -- error.
        Left err@TextEnvelopeTypeError{} ->
          DeserialiseInputError (InputTextEnvelopeError err)

        -- The input was not valid a text envelope.
        Left _ -> DeserialiseInputErrorFormatMismatch

    deserialiseBech32 :: DeserialiseInputResult b
    deserialiseBech32 =
      case deserialiseAnyOfFromBech32 bech32Types inputText of
        Right res -> DeserialiseInputSuccess res

        -- The input was not valid Bech32.
        Left (Bech32DecodingError _) -> DeserialiseInputErrorFormatMismatch

        -- The input was valid Bech32, but some other error occurred.
        Left err -> DeserialiseInputError $ InputBech32DecodeError err

------------------------------------------------------------------------------
-- Encoded file deserialisation
------------------------------------------------------------------------------

-- | Read and decode input from a file.
--
-- The contents of the file can either be Bech32-encoded, hex-encoded, or in
-- the text envelope format.
readFileAnyOfInputFormats
  :: AsType a
  -> [InputFormat a]
  -> FilePath
  -> IO (Either (FileError InputDecodeError) a)
readFileAnyOfInputFormats asType acceptedFormats path =
  runExceptT $ do
    content <- handleIOExceptT (FileIOError path) $ BS.readFile path
    firstExceptT (FileError path) $ hoistEither $
      deserialiseInput asType acceptedFormats content

-- | Read and decode input from a text envelope file.
--
-- The contents of the file must be in the text envelope format.
readFileTextEnvelope'
  :: HasTextEnvelope a
  => AsType a
  -> FilePath
  -> IO (Either (FileError InputDecodeError) a)
readFileTextEnvelope' asType fp =
    first toInputDecodeError <$> readFileTextEnvelope asType fp
  where
    toInputDecodeError
      :: FileError TextEnvelopeError
      -> FileError InputDecodeError
    toInputDecodeError err =
      case err of
        FileIOError path ex -> FileIOError path ex
        FileError path textEnvErr ->
          FileError path (InputTextEnvelopeError textEnvErr)
        FileErrorTempFile targetP tempP h ->
          FileErrorTempFile targetP tempP h

-- | Read and decode input from a file given that it is one of the provided
-- types.
--
-- The contents of the file can either be Bech32-encoded or in the text
-- envelope format.
readFileBech32OrTextEnvAnyOf
  :: forall b.
     [FromSomeType SerialiseAsBech32 b]
  -> [FromSomeType HasTextEnvelope b]
  -> FilePath
  -> IO (Either (FileError InputDecodeError) b)
readFileBech32OrTextEnvAnyOf bech32Types textEnvTypes path =
  runExceptT $ do
    content <- handleIOExceptT (FileIOError path) $ BS.readFile path
    firstExceptT (FileError path) $ hoistEither $
      deserialiseInputAnyOf bech32Types textEnvTypes content
