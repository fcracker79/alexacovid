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
import Control.Monad.Trans.Except(ExceptT(..), runExceptT)
import Control.Monad.IO.Class(liftIO)


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
    
getRegionColor :: AlexaRequest -> Context () -> IO (Either AlexaResponse AlexaResponse)
getRegionColor r c = do
    runExceptT $ eitherRegionColor r c


eitherGeolocation :: AlexaRequest -> ExceptT AlexaResponse IO (Float, Float)
eitherGeolocation r = do
    let maybeGeolocation = fmap ((\c -> (latitudeInDegrees c, longitudeInDegrees c)) . coordinate) ((alexaGeolocation . context) r)
    case maybeGeolocation of
        Nothing -> createErrorResponse "Abilita servizi geografici"
        Just geolocation -> return geolocation


eitherItalianRegion :: String -> (Float, Float) -> ExceptT AlexaResponse IO ItalianRegion
eitherItalianRegion geoServiceKey coords = do
    maybeItalianRegion <- liftIO $ runReaderT (runMaybeT (getRegion coords)) geoServiceKey
    case maybeItalianRegion of
        Nothing -> createErrorResponse "Non ho trovato la tua regione" 
        Just italianRegion -> return italianRegion


eitherRegionColorByRegion :: ItalianRegion -> ExceptT AlexaResponse IO AlexaResponse
eitherRegionColorByRegion italianRegion = do
    maybeRegionColors <- liftIO $ runMaybeT getRegionColors
    case fmap (Map.lookup italianRegion) maybeRegionColors of
        Just (Just color) -> createColorResponse color
        _ -> createErrorResponse "Non ho trovato il colore per la tua regione" 
        
eitherRegionColor :: AlexaRequest -> Context () -> ExceptT AlexaResponse IO AlexaResponse
eitherRegionColor r _ = do
    coordsFromRequest <- eitherGeolocation r
    awsRegion <- liftIO getAWSRegion    
    geoServiceKey <- liftIO $ runReaderT getGeoServiceKey awsRegion
    italianRegion <- eitherItalianRegion geoServiceKey coordsFromRequest
    eitherRegionColorByRegion italianRegion


createColorResponse :: String -> ExceptT AlexaResponse IO AlexaResponse
createColorResponse color = ExceptT (pure resp)
    where resp = Right AlexaResponse {
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

createErrorResponse :: String -> ExceptT AlexaResponse IO v
createErrorResponse message = ExceptT (pure resp)
    where resp = Left AlexaResponse {
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