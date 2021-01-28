module AWS.AWSTypes.AlexaMessages where

{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}


import qualified Data.Map as Map
import Data.Aeson
    ( genericParseJSON,
      defaultOptions,
      genericToJSON,
      FromJSON(parseJSON),
      Options(fieldLabelModifier),
      ToJSON(toJSON),
      Value )
import GHC.Generics (Generic)
import AWS.AWSTypes.AlexaContext(AlexaContext)


newtype AlexaStatus = AlexaStatus { code :: String} deriving (Generic, Show, FromJSON, ToJSON)
data AlexaValue = AlexaValue {
    name :: String, value :: String
} deriving (Generic, Show, FromJSON, ToJSON)

data AlexaResolutionPerAuthority = AlexaResolutionPerAuthority {
    authority :: String,
    status :: AlexaStatus,
    values :: [AlexaValue]
} deriving (Generic, Show, FromJSON, ToJSON)


data AlexaIntentSlot = AlexaIntentSlot {
    slotname :: String,
    slotvalue :: String,
    slotconfirmationStatus :: String,
    slotresolutions :: [AlexaResolutionPerAuthority]
} deriving (Generic, Show)

instance ToJSON AlexaIntentSlot where
    toJSON = genericToJSON defaultOptions {
                fieldLabelModifier = drop 4 }

instance FromJSON AlexaIntentSlot where
    parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 4 }

data AlexaIntent = AlexaIntent {
    intentname :: String,
    intentconfirmationStatus :: String,
    intentslots :: Maybe (Map.Map String AlexaIntentSlot)
} deriving (Generic, Show)
instance ToJSON AlexaIntent where
    toJSON = genericToJSON defaultOptions {
                fieldLabelModifier = drop 6 }

instance FromJSON AlexaIntent where
    parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 6 }


data AlexaRequestPayload = AlexaRequestPayload {
    _type :: String,
    _requestId :: String,
    _timestamp :: String,
    _locale :: String,
    _intent :: Maybe AlexaIntent
    } deriving (Generic, Show)

instance ToJSON AlexaRequestPayload where
    toJSON = genericToJSON defaultOptions {
                fieldLabelModifier = drop 1 }

instance FromJSON AlexaRequestPayload where
    parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 1 }
    
data AlexaRequest = AlexaRequest {
    request :: AlexaRequestPayload,
    context :: AlexaContext
} deriving (Generic, Show, FromJSON, ToJSON)

type AlexaDirective = Value

newtype AlexaReprompt = AlexaReprompt {
    _outputSpeech :: AlexaOutputSpeech
} deriving (Generic, Show)

instance ToJSON AlexaReprompt where
    toJSON = genericToJSON defaultOptions {
                fieldLabelModifier = drop 1 }

instance FromJSON AlexaReprompt where
    parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 1 }



data AlexaCard = AlexaCard {
    ctype :: String,
    ctitle :: String,
    ctext :: String
} deriving (Generic, Show)

instance ToJSON AlexaCard where
    toJSON = genericToJSON defaultOptions {
                fieldLabelModifier = drop 1 }

instance FromJSON AlexaCard where
    parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 1 }


data AlexaOutputSpeech = AlexaOutputSpeech {
    aostype :: String,
    aostext :: String,
    aosplayBehavior :: String
} deriving (Generic, Show)

instance ToJSON AlexaOutputSpeech where
    toJSON = genericToJSON defaultOptions {
                fieldLabelModifier = drop 3 }

instance FromJSON AlexaOutputSpeech where
    parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 3 }


data AlexaResponse = AlexaResponse {
    version :: String,
    response :: AlexaResponsePayload
} deriving (Generic, Show, FromJSON, ToJSON)

data AlexaResponsePayload = AlexaResponsePayload {
    outputSpeech :: AlexaOutputSpeech,
    card :: AlexaCard,
    reprompt :: Maybe AlexaReprompt,
    directives :: [AlexaDirective],
    shouldEndSession :: Bool
} deriving (Generic, Show, FromJSON, ToJSON)