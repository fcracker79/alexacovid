module AWS.AWSFunctions where

import AWS.AWSTypes.AlexaMessages
import Regions(ItalianRegion)
import Geo.GeoService(getRegion)
import Geo.GeoCredentials(getGeoServiceKey)
import qualified System.Environment as Sysenv
import Control.Monad.Reader(runReaderT)
import Control.Monad.Trans.Maybe(runMaybeT)
import Network.AWS
import Network.AWS.Data
import Data.Text(pack)
import Aws.Lambda ( Context(..) )
import RegionColors(getRegionColors)
import qualified Data.Map as Map
import AWS.AWSTypes.AlexaContext
import Data.IORef(readIORef)
import Data.Aeson
import Data.ByteString.Lazy.Char8(unpack)

debugMessage :: Value -> Context () -> IO (Either String AlexaResponse)
debugMessage r c = do
    let s = unpack (encode r)
    print $ "THIS IS MY ENTIRE MESSAGE " ++ s
    return $ Left s


getAWSRegion :: IO Region
getAWSRegion = do
    regionString <- Sysenv.getEnv "AWS_REGION"
    case fromText (pack regionString)::(Either String Region) of
        Left x -> ioError (userError x)
        Right x -> return x
    
getRegionColor :: AlexaRequest -> Context () -> IO (Either String AlexaResponse)
getRegionColor r _ = do
    case getCoordinates of
        Nothing -> return $ createErrorResponse "Abilita servizi geografici"
        Just coords -> getRegionColorByCoords coords
    where maybeCoords = coordinate <$> (alexaGeolocation . context) r
          getCoordinates = fmap (\jcoords -> (latitudeInDegrees jcoords, longitudeInDegrees jcoords)) maybeCoords


getRegionColorByCoords :: (Float, Float) -> IO (Either String AlexaResponse)
getRegionColorByCoords coords = do
    awsRegion <- getAWSRegion    
    geoServiceKey <- runReaderT getGeoServiceKey awsRegion
    maybeItalianRegion <- runReaderT (runMaybeT (getRegion coords)) geoServiceKey
    case maybeItalianRegion of
        Nothing -> return $ createErrorResponse "Non ho trovato la tua regione" 
        Just italianRegion -> getRegionColorByRegion italianRegion

getRegionColorByRegion :: ItalianRegion -> IO (Either String AlexaResponse)
getRegionColorByRegion italianRegion = do
    maybeRegionColors <- runMaybeT getRegionColors
    case fmap (Map.lookup italianRegion) maybeRegionColors of
        Just (Just color) -> return $ createColorResponse color
        _ -> return $ createErrorResponse "Non ho trovato il colore per la tua regione" 


createColorResponse :: String -> Either String AlexaResponse
createColorResponse color = Right AlexaResponse {
    outputSpeech = AlexaOutputSpeech {
        aostype = "PlainText",
        aostext = "Oggi il colore della tua regione è " ++ color,
        aosplayBehavior = "REPLACE_ENQUEUED"
    },
    card = AlexaCard {
        ctype = "Standard",
        ctitle = "Colori CoVid delle regioni",
        ctext = "Scopri giorno per giorno il colore della tua regione"
    },
    reprompt = Nothing,
    directives = [],
    shouldEndSession = True
}

createErrorResponse :: String -> Either String AlexaResponse
createErrorResponse message = Right AlexaResponse {
    outputSpeech = AlexaOutputSpeech {
        aostype = "PlainText",
        aostext = message,
        aosplayBehavior = "REPLACE_ENQUEUED"
    },
    card = AlexaCard {
        ctype = "Standard",
        ctitle = message,
        ctext = message
    },
    reprompt = Nothing,
    directives = [],
    shouldEndSession = True
}