module AWS.AWSFunctions where

import AWS.AlexaMessages
import Regions(ItalianRegion)
import Geo.GeoService(getRegion)
import Geo.GeoCredentials(getGeoServiceKey)
import qualified System.Environment as Sysenv
import Control.Monad.Reader(runReaderT)
import Control.Monad.Trans.Maybe(runMaybeT)
import Network.AWS
import Network.AWS.Data
import Data.Text(pack)
import Aws.Lambda ( Context )
import RegionColors(getRegionColors)
import qualified Data.Map as Map


getAWSRegion :: IO Region
getAWSRegion = do
    regionString <- Sysenv.getEnv "AWS_REGION"
    case fromText (pack regionString)::(Either String Region) of
        Left x -> ioError (userError x)
        Right x -> return x
    
getRegionColor :: AlexaRequest -> Context () -> IO (Either String AlexaResponse)
getRegionColor r c = do
    awsRegion <- getAWSRegion
    geoServiceKey <- runReaderT getGeoServiceKey awsRegion
    -- TODO verify how to extract coordinates from Context
    maybeItalianRegion <- runReaderT (runMaybeT (getRegion (42.8, 10.6))) geoServiceKey
    case maybeItalianRegion of
        Nothing -> return $ Left "No such region for position"
        Just italianRegion -> regionColorResponse italianRegion
    where regionColorResponse italianRegion = do
            maybeRegions <- runMaybeT getRegionColors
            case maybeRegions of
                Nothing -> return $ Left "No such region colors"
                Just regionColors ->
                    case Map.lookup italianRegion regionColors of
                        Nothing -> return $ Left ("Missing color for region " ++ show italianRegion)
                        Just color -> return $ createColorResponse color

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