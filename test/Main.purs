module Test.Main
  ( main
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Generic.Rep (class DecodeLiteral, class DecodeRep, decodeLiteralSumWithTransform, genericDecodeJson, genericDecodeJsonWith)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Generic.Rep (class EncodeLiteral, class EncodeRep, encodeLiteralSumWithTransform, genericEncodeJson, genericEncodeJsonWith)
import Data.Argonaut.Options (Options, constructorTagModifierL, contentsFieldNameL, defaultOptions, fieldLabelModifierL, sumEncodingL, tagFieldNameL, taggedObjectL)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..), fromRight)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens ((.~))
import Data.String (toLower, toUpper)
import Partial.Unsafe (unsafePartial)
import Test.Assert (ASSERT, assert)

data Example
  = Either (Either String Example)
  | Record {foo :: Int, bar :: String}
  | Product Int Int Example

derive instance eqExample :: Eq Example
derive instance genericExample :: Generic Example _
instance showExample :: Show Example where
  show a = genericShow a
instance encodeJsonExample :: EncodeJson Example where
  encodeJson a = genericEncodeJson a
instance decodeJson :: DecodeJson Example where
  decodeJson a = genericDecodeJson a

data LiteralStringExample
  = Apple
  | Banana
  | Frikandel

derive instance eqLiteralStringExample :: Eq LiteralStringExample
derive instance genericLiteralStringExample :: Generic LiteralStringExample _
instance showLiteralStringExample :: Show LiteralStringExample where
  show a = genericShow a
instance encodeJsonLiteralStringExample :: EncodeJson LiteralStringExample where
  encodeJson a = encodeLiteralSumWithTransform id a
instance decodeJsonLiteralStringExample :: DecodeJson LiteralStringExample where
  decodeJson a = decodeLiteralSumWithTransform id a

main :: forall eff. Eff (assert :: ASSERT, console :: CONSOLE | eff) Unit
main = do
  example $ Either $ Left "foo"
  example $ Either $ Right $ Either $ Left "foo"
  example $ Record {foo: 42, bar: "bar"}
  example $ Product 1 2 $ Either $ Left "foo"
  example $ Frikandel
  testLiteralSumWithTransform id Frikandel "\"Frikandel\""
  testLiteralSumWithTransform toUpper Frikandel "\"FRIKANDEL\""
  testLiteralSumWithTransform toLower Frikandel "\"frikandel\""

  log $ "NOOP"
  testEncodingMatches
    defaultOptions
    "{\"values\":[{\"foo\":42,\"bar\":\"DRINK\"}],\"tag\":\"Record\"}"
    $ Record {foo: 42, bar: "DRINK"}

  log $ "Changing values tag name"
  testEncodingMatches
    (defaultOptions # sumEncodingL <<< taggedObjectL <<< tagFieldNameL .~ "day")
    "{\"values\":[{\"foo\":42,\"bar\":\"DRINK\"}],\"day\":\"Record\"}"
    $ Record {foo: 42, bar: "DRINK"}

  log $ "Upper case type constructor"
  testEncodingMatches
    (defaultOptions # constructorTagModifierL .~ toUpper)
    "{\"values\":[{\"foo\":42,\"bar\":\"DRINK\"}],\"tag\":\"RECORD\"}"
    $ Record {foo: 42, bar: "DRINK"}

  log $ "Upper case field names"
  testEncodingMatches
    (defaultOptions # fieldLabelModifierL .~ toUpper)
    "{\"values\":[{\"FOO\":42,\"BAR\":\"DRINK\"}],\"tag\":\"Record\"}"
    $ Record {foo: 42, bar: "DRINK"}

  where
  example :: forall a. Show a => Eq a => EncodeJson a => DecodeJson a => a -> Eff _ Unit
  example original = do
    let json = encodeJson original
    let parsed = decodeJson json
    log $ "Original:  " <> show original
    log $ "To JSON:   " <> stringify json
    log $ "From JSON: " <> show parsed
    assert $ parsed == Right original
    log $ "--------------------------------------------------------------------------------"

  testLiteralSumWithTransform :: forall a rep
     . Show a
    => Eq a
    => Generic a rep
    => EncodeLiteral rep
    => DecodeLiteral rep
    => (String -> String)
    -> a
    -> String
    -> Eff _ Unit
  testLiteralSumWithTransform tagNameTransform original string = do
    let json = encodeLiteralSumWithTransform tagNameTransform original
    let parsed = decodeLiteralSumWithTransform tagNameTransform json
    let parsed' = decodeLiteralSumWithTransform tagNameTransform <<< unsafePartial fromRight $ jsonParser string
    log $ "Original:  " <> show original
    log $ "To JSON:   " <> stringify json
    log $ "From JSON: " <> show parsed
    assert $ parsed == Right original
    assert $ parsed' == Right original
    log $ "--------------------------------------------------------------------------------"

  testEncodingMatches :: forall a rep
     . Show a
    => Eq a
    => DecodeJson a
    => Generic a rep
    => EncodeRep rep
    => DecodeRep rep
    => Options
    -> String
    -> a
    -> Eff _ Unit
  testEncodingMatches options expected original = do
    let json = stringify $ genericEncodeJsonWith options original
    let decoded = genericDecodeJsonWith options =<< jsonParser expected
    log $ "Original: " <> show original
    log $ "Encoded:  " <> json
    log $ "Expected: " <> expected
    assert $ json == expected
    log $ "Decoded:  " <> show decoded
    assert $ decoded == Right original
