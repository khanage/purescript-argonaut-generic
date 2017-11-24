module Data.Argonaut.Options
       ( Options(..)
       , SumEncoding(..)
       , TaggedObjectR
       , defaultOptions
       , defaultSumEncoding
       , fieldLabelModifierL, constructorTagModifierL, sumEncodingL
       , taggedObjectL, tagFieldNameL, contentsFieldNameL
       ) where

import Prelude

import Data.Lens (Lens', Prism', lens, prism')
import Data.Maybe (Maybe(..))

data Options = Options
  { fieldLabelModifier     :: String -> String
  , constructorTagModifier :: String -> String
  , sumEncoding            :: SumEncoding
  }

fieldLabelModifierL :: Lens' Options (String -> String)
fieldLabelModifierL = lens (\(Options o) -> o.fieldLabelModifier)
                           (\(Options o) modifier -> Options (o { fieldLabelModifier = modifier}))

constructorTagModifierL :: Lens' Options (String -> String)
constructorTagModifierL = lens (\(Options o) -> o.constructorTagModifier)
                               (\(Options o) modifier -> Options (o {constructorTagModifier = modifier}))

sumEncodingL :: Lens' Options SumEncoding
sumEncodingL = lens (\(Options o) -> o.sumEncoding)
                    (\(Options o) encoding -> Options (o { sumEncoding = encoding }))

defaultOptions :: Options
defaultOptions = Options
  { fieldLabelModifier     : id
  , constructorTagModifier : id
  , sumEncoding            : defaultSumEncoding
  }

type TaggedObjectR = { tagFieldName :: String, contentsFieldName :: String }
data SumEncoding = TaggedObject TaggedObjectR

taggedObjectL :: Prism' SumEncoding TaggedObjectR
taggedObjectL = prism' TaggedObject \encoding ->
  case encoding of
    TaggedObject props -> Just props

tagFieldNameL :: Lens' TaggedObjectR String
tagFieldNameL = lens (\r -> r.tagFieldName) (\r s -> r { tagFieldName = s})

contentsFieldNameL :: Lens' TaggedObjectR String
contentsFieldNameL = lens (\r -> r.contentsFieldName) (\r s -> r { contentsFieldName = s })

defaultSumEncoding :: SumEncoding
defaultSumEncoding = TaggedObject { tagFieldName: "tag", contentsFieldName: "values" }
