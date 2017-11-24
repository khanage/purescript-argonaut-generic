module Data.Argonaut.Decode.Generic.Rep (
  class DecodeRep,
  class DecodeRepArgs,
  class DecodeRepFields,
  class DecodeLiteral,
  decodeRep,
  decodeRepArgs,
  decodeRepFields,
  genericDecodeJson,
  genericDecodeJsonWith,
  decodeLiteralSum,
  decodeLiteralSumWithTransform,
  decodeLiteral
) where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut.Core (Json, toArray, toObject, toString)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Options (Options(..), contentsFieldNameL, defaultOptions, fieldLabelModifierL, sumEncodingL, tagFieldNameL, taggedObjectL)
import Data.Array (uncons)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Generic.Rep as Rep
import Data.Lens ((^?), (^.))
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.StrMap as SM
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Partial.Unsafe (unsafeCrashWith)

class DecodeRep r where
  decodeRep :: Options -> Json -> Either String r

instance decodeRepNoConstructors :: DecodeRep Rep.NoConstructors where
  decodeRep _ _ = Left "Cannot decode empty data type"

instance decodeRepSum :: (DecodeRep a, DecodeRep b) => DecodeRep (Rep.Sum a b) where
  decodeRep o j = Rep.Inl <$> decodeRep o j <|> Rep.Inr <$> decodeRep o j

instance decodeRepConstructor :: (IsSymbol name, DecodeRepArgs a) => DecodeRep (Rep.Constructor name a) where
  decodeRep o@(Options options) j = do
    let name = options.constructorTagModifier $ reflectSymbol (SProxy :: SProxy name)
    let decodingErr msg = "When decoding a " <> name <> ": " <> msg
    let tagFieldName = fromMaybe "tag" $ o ^? sumEncodingL <<< taggedObjectL <<< tagFieldNameL
    let contentsFieldName = fromMaybe "values" $ o ^? sumEncodingL <<< taggedObjectL <<< contentsFieldNameL
    jObj <- mFail (decodingErr "expected an object") (toObject j)
    jTag <- mFail (decodingErr "'" <> tagFieldName <> "' property is missing") (SM.lookup tagFieldName jObj)
    tag  <- mFail (decodingErr "'" <> tagFieldName <> "' property is not a string") (toString jTag)
    when (tag /= name) $
      Left $ decodingErr "'" <> tagFieldName <> "' property has an incorrect value, expected (" <> name <> "), found (" <> tag <> ")"
    jValues <- mFail (decodingErr "'" <> contentsFieldName <> "' property is missing") (SM.lookup contentsFieldName jObj)
    values  <- mFail (decodingErr "'" <> contentsFieldName <> "' property is not an array") (toArray jValues)
    {init, rest} <- lmap decodingErr $ decodeRepArgs o values
    when (rest /= []) $
      Left $ decodingErr "'" <> contentsFieldName <> "' property had too many values"
    pure $ Rep.Constructor init

class DecodeRepArgs r where
  decodeRepArgs :: Options -> Array Json -> Either String {init :: r, rest :: Array Json}

instance decodeRepArgsNoArguments :: DecodeRepArgs Rep.NoArguments where
  decodeRepArgs o js = Right {init: Rep.NoArguments, rest: js}

instance decodeRepArgsProduct :: (DecodeRepArgs a, DecodeRepArgs b) => DecodeRepArgs (Rep.Product a b) where
  decodeRepArgs o js = do
    {init: a, rest: js'} <- decodeRepArgs o js
    {init: b, rest: js''} <- decodeRepArgs o js'
    pure {init: Rep.Product a b, rest: js''}

instance decodeRepArgsArgument :: (DecodeJson a) => DecodeRepArgs (Rep.Argument a) where
  decodeRepArgs o js = do
    {head, tail} <- mFail "too few values were present" (uncons js)
    {init: _, rest: tail} <<< Rep.Argument <$> decodeJson head

instance decodeRepArgsRec :: (DecodeRepFields fields) => DecodeRepArgs (Rep.Rec fields) where
  decodeRepArgs o js = do
    {head, tail} <- mFail "too few values were present" (uncons js)
    jObj <- mFail "record is not encoded as an object" (toObject head)
    {init: _, rest: tail} <<< Rep.Rec <$> decodeRepFields o jObj

class DecodeRepFields r where
  decodeRepFields :: Options -> SM.StrMap Json -> Either String r

instance decodeRepFieldsProduct :: (DecodeRepFields a, DecodeRepFields b) => DecodeRepFields (Rep.Product a b) where
  decodeRepFields o js = Rep.Product <$> decodeRepFields o js <*> decodeRepFields o js

instance decodeRepFieldsField :: (IsSymbol field, DecodeJson a) => DecodeRepFields (Rep.Field field a) where
  decodeRepFields options js = do
    let name = options ^. fieldLabelModifierL $ reflectSymbol (SProxy :: SProxy field)
    value <- mFail ("the field '" <> name <> "' is not present") (SM.lookup name js)
    Rep.Field <$> decodeJson value

-- | Decode `Json` representation of a value which has a `Generic` type.
genericDecodeJson :: forall a r. Rep.Generic a r => DecodeRep r => Json -> Either String a
genericDecodeJson = genericDecodeJsonWith defaultOptions

genericDecodeJsonWith :: forall a r. Rep.Generic a r => DecodeRep r => Options -> Json -> Either String a
genericDecodeJsonWith options = map Rep.to <<< decodeRep options

mFail :: forall a. String -> Maybe a -> Either String a
mFail msg = maybe (Left msg) Right

-- | A function for decoding `Generic` sum types using string literal representations
decodeLiteralSum :: forall a r. Rep.Generic a r => DecodeLiteral r => Json -> Either String a
decodeLiteralSum = decodeLiteralSumWithTransform id

-- | A function for decoding `Generic` sum types using string literal representations
-- | Takes a function for transforming the tag name in encoding
decodeLiteralSumWithTransform :: forall a r. Rep.Generic a r => DecodeLiteral r => (String -> String) -> Json -> Either String a
decodeLiteralSumWithTransform tagNameTransform = map Rep.to <<< decodeLiteral tagNameTransform

class DecodeLiteral r where
  decodeLiteral :: (String -> String) -> Json -> Either String r

instance decodeLiteralSumInst :: (DecodeLiteral a, DecodeLiteral b) => DecodeLiteral (Rep.Sum a b) where
  decodeLiteral tagNameTransform j = Rep.Inl <$> decodeLiteral tagNameTransform j <|> Rep.Inr <$> decodeLiteral tagNameTransform j

instance decodeLiteralConstructor :: (IsSymbol name) => DecodeLiteral (Rep.Constructor name (Rep.NoArguments)) where
  decodeLiteral tagNameTransform j = do
    let name = reflectSymbol (SProxy :: SProxy name)
    let decodingErr msg = "When decoding a " <> name <> ": " <> msg
    tag <- mFail (decodingErr "could not read string for constructor") (toString j)
    when (tag /= tagNameTransform name) $
      Left $ decodingErr "string literal " <> tag <> " had an incorrect value."
    pure $ Rep.Constructor (Rep.NoArguments)

type FailMessage = """`decodeLiteralSum` can only be used with sum types, where all of the constructors are nullary. This is because a string literal cannot be encoded into a product type."""

instance decodeLiteralConstructorCannotTakeProduct
  :: Fail FailMessage
  => DecodeLiteral (Rep.Product a b) where
    decodeLiteral _ _ = unsafeCrashWith "unreachable DecodeLiteral was reached."
