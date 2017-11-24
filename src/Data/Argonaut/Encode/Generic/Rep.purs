module Data.Argonaut.Encode.Generic.Rep (
  class EncodeRep,
  class EncodeRepArgs,
  class EncodeRepFields,
  class EncodeLiteral,
  encodeRepOptions,
  encodeRepArgs,
  encodeRepFields,
  genericEncodeJson,
  genericEncodeJsonWith,
  encodeLiteralSum,
  encodeLiteralSumWithTransform,
  encodeLiteral
) where

import Prelude

import Data.Argonaut.Core (Json, fromArray, fromObject, fromString)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Options (Options(..), SumEncoding(..), defaultOptions)
import Data.Generic.Rep as Rep
import Data.StrMap as SM
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Partial.Unsafe (unsafeCrashWith)

class EncodeRep r where
  encodeRepOptions :: Options -> r -> Json

instance encodeRepNoConstructors :: EncodeRep Rep.NoConstructors where
  encodeRepOptions o r = encodeRepOptions o r

instance encodeRepSum :: (EncodeRep a, EncodeRep b) => EncodeRep (Rep.Sum a b) where
  encodeRepOptions options (Rep.Inl a) = encodeRepOptions options a
  encodeRepOptions options (Rep.Inr b) = encodeRepOptions options b

instance encodeRepConstructor :: (IsSymbol name, EncodeRepArgs a) => EncodeRep (Rep.Constructor name a) where
  encodeRepOptions o@(Options options) (Rep.Constructor a) =
    case options.sumEncoding of
      TaggedObject {tagFieldName, contentsFieldName } ->
        fromObject
          $ SM.insert tagFieldName (fromString (options.constructorTagModifier (reflectSymbol (SProxy :: SProxy name))))
          $ SM.insert contentsFieldName (fromArray (encodeRepArgs o a))
          $ SM.empty

class EncodeRepArgs r where
  encodeRepArgs :: Options -> r -> Array Json

instance encodeRepArgsNoArguments :: EncodeRepArgs Rep.NoArguments where
  encodeRepArgs _ Rep.NoArguments = []

instance encodeRepArgsProduct :: (EncodeRepArgs a, EncodeRepArgs b) => EncodeRepArgs (Rep.Product a b) where
  encodeRepArgs o (Rep.Product a b) = encodeRepArgs o a <> encodeRepArgs o b

instance encodeRepArgsArgument :: (EncodeJson a) => EncodeRepArgs (Rep.Argument a) where
  encodeRepArgs _ (Rep.Argument a) = [encodeJson a]

instance encodeRepArgsRec :: (EncodeRepFields fields) => EncodeRepArgs (Rep.Rec fields) where
  encodeRepArgs o (Rep.Rec fields) = [fromObject $ encodeRepFields o fields]

class EncodeRepFields r where
  encodeRepFields :: Options -> r -> SM.StrMap Json

instance encodeRepFieldsProduct :: (EncodeRepFields a, EncodeRepFields b) => EncodeRepFields (Rep.Product a b) where
  encodeRepFields o (Rep.Product a b) =
    SM.union (encodeRepFields o a) (encodeRepFields o b)

instance encodeRepFieldsField :: (IsSymbol field, EncodeJson a) => EncodeRepFields (Rep.Field field a) where
  encodeRepFields (Options options) (Rep.Field a) =
    SM.singleton (options.fieldLabelModifier (reflectSymbol (SProxy :: SProxy field)))
                 (encodeJson a)

-- | Encode any `Generic` data structure into `Json`.
genericEncodeJson :: forall a r. Rep.Generic a r => EncodeRep r => a -> Json
genericEncodeJson = genericEncodeJsonWith defaultOptions

-- | Encode any `Generic` data structure into `Json` with options controlling serialisation.
genericEncodeJsonWith :: forall a r. Rep.Generic a r => EncodeRep r => Options -> a -> Json
genericEncodeJsonWith options = encodeRepOptions options <<< Rep.from

-- | A function for encoding `Generic` sum types using string literal representations
encodeLiteralSum :: forall a r. Rep.Generic a r => EncodeLiteral r => a -> Json
encodeLiteralSum = encodeLiteralSumWithTransform id

-- | A function for encoding `Generic` sum types using string literal representations
-- | Takes a function for transforming the tag name in encoding
encodeLiteralSumWithTransform :: forall a r. Rep.Generic a r => EncodeLiteral r => (String -> String) -> a -> Json
encodeLiteralSumWithTransform tagNameTransform = encodeLiteral tagNameTransform <<< Rep.from

class EncodeLiteral r where
  encodeLiteral :: (String -> String) -> r -> Json

instance encodeLiteralSumInst :: (EncodeLiteral a, EncodeLiteral b) => EncodeLiteral (Rep.Sum a b) where
  encodeLiteral tagNameTransform (Rep.Inl a) = encodeLiteral tagNameTransform a
  encodeLiteral tagNameTransform (Rep.Inr b) = encodeLiteral tagNameTransform b

instance encodeLiteralConstructor :: (IsSymbol name) => EncodeLiteral (Rep.Constructor name (Rep.NoArguments)) where
  encodeLiteral tagNameTransform _ = fromString <<< tagNameTransform $ reflectSymbol (SProxy :: SProxy name)

type FailMessage = """`encodeLiteralSum` can only be used with sum types, where all of the constructors are nullary. This is because a string literal cannot be encoded into a product type."""

instance encodeLiteralConstructorCannotBeProduct
  :: Fail FailMessage
  => EncodeLiteral (Rep.Product a b) where
  encodeLiteral _ _ = unsafeCrashWith "unreachable encodeLiteral was reached."
