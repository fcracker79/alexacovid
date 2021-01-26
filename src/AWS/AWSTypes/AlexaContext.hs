module AWS.AWSTypes.AlexaContext where


{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}


import Data.Aeson
import GHC.Generics (Generic)


data Speed = Speed {
    speedInMetersPerSecond :: Float,
    accuracyInMetresPerSecond :: Float
} deriving(Generic, Show, Eq, FromJSON, ToJSON)

data Heading = Heading {
    directionInDegrees :: String,
    accuracyInDegrees :: String
} deriving(Generic, Show, Eq, FromJSON, ToJSON)

data Altitude = Altitude {
    _altitudeInMeters :: Float,
    _accuracyInMeters :: Float
} deriving(Generic, Show, Eq)
instance ToJSON Altitude where
    toJSON = genericToJSON defaultOptions {
                fieldLabelModifier = drop 1 }

instance FromJSON Altitude where
    parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 1 }

data Coordinate = Coordinate {
    latitudeInDegrees :: Float,
    longitudeInDegrees :: Float,
    accuracyInMeters :: Float
} deriving(Generic, Show, Eq, FromJSON, ToJSON)

data LocationServices = LocationServices {
    access :: String,
    status :: String
} deriving(Generic, Show, Eq, FromJSON, ToJSON)


data GeoLocation = GeoLocation {
    locationServices :: LocationServices,
    timestamp :: String,
    coordinate :: Coordinate,
    altitude :: Altitude,
    heading :: Heading,
    speed :: Speed
} deriving(Generic, Show, Eq, FromJSON, ToJSON)

newtype SupportedInterfaces = SupportedInterfaces {
    _Geolocation :: Bool
} deriving(Generic, Show, Eq)
instance ToJSON SupportedInterfaces where
    toJSON = genericToJSON defaultOptions {
                fieldLabelModifier = drop 1 }

instance FromJSON SupportedInterfaces where
    parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 1 }

newtype Device = Device {
    supportedInterfaces :: SupportedInterfaces
} deriving(Generic, Show, Eq, FromJSON, ToJSON)

newtype System = System {
    device :: Device
} deriving(Generic, Show, Eq, FromJSON, ToJSON)

data AlexaContext = AlexaContext {
    alexaGeolocation :: Maybe GeoLocation,
    alexaSystem :: System
} deriving(Generic, Show, Eq)
instance ToJSON AlexaContext where
    toJSON = genericToJSON defaultOptions {
                fieldLabelModifier = drop 5 }

instance FromJSON AlexaContext where
    parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 5 }
