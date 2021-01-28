module AWS.AWSFunctions(getRegionColor, debugMessage) where

import Control.Applicative((<|>))
import AWS.AWSTypes.AlexaMessages
    ( AlexaResponsePayload(AlexaResponsePayload, outputSpeech, card,
                           reprompt, directives, shouldEndSession),
      AlexaResponse(..),
      AlexaOutputSpeech(AlexaOutputSpeech, aostype, aostext,
                        aosplayBehavior),
      AlexaCard(AlexaCard, ctype, ctitle, ctext),
      AlexaRequest(context, session) )
import Regions(ItalianRegion, regionByCap)
import Geo.GeoService(getRegion)
import Geo.GeoCredentials(getGeoServiceKey)
import qualified System.Environment as Sysenv
import Control.Monad.Reader(runReaderT)
import Control.Monad.Trans.Maybe(runMaybeT)
import Network.AWS ( Region )
import Network.AWS.Data ( fromText )
import Data.Text(pack)
import Aws.Lambda ( Context(..) )
import RegionColors(getRegionColors)
import qualified Data.Map as Map
import AWS.AWSTypes.AlexaContext
    ( AlexaContext(alexaGeolocation, alexaSystem),
      System(device),
      Device(deviceId),
      GeoLocation(coordinate),
      Coordinate(latitudeInDegrees, longitudeInDegrees) )
import AWS.AWSTypes.AlexaSession
    ( AlexaSession(user),
      User(permissions),
      Permissions(consentToken) )
import Data.IORef(readIORef)
import qualified Data.ByteString.Char8 as Char8
import Control.Monad.Trans.Except(ExceptT(..), runExceptT)
import Control.Monad.IO.Class(liftIO)
import Debug.Trace(trace)
import Data.Aeson ( Value, FromJSON, ToJSON, encode )
import GHC.Generics(Generic)
import Network.HTTP.Req
    ( (/:),
      defaultHttpConfig,
      header,
      https,
      jsonResponse,
      req,
      responseBody,
      runReq,
      GET(GET),
      HttpException,
      NoReqBody(NoReqBody) )
import qualified Data.Text as Text
import qualified Data.ByteString as ByteString
import Control.Exception(catch)


debugMessage :: Value -> Context () -> IO (Either String AlexaResponse)
debugMessage r c = do
    print $ "THIS IS MY ENTIRE MESSAGE " ++ (show . encode) r
    return $ Left "DEBUG MESSAGE"


instance Semigroup AlexaResponse where
    a <> b = newResponseMessage (ma ++ mb)
             where ma = (aostext . outputSpeech . response) a
                   mb = (aostext . outputSpeech . response) b
instance Monoid AlexaResponse where
    mempty = newResponseMessage ""


newtype AlexaCAPResponse = AlexaCAPResponse {
    postalCode :: String
} deriving(Show, Eq, Generic, FromJSON, ToJSON)

getAWSRegion :: IO Region
getAWSRegion = do
    regionString <- Sysenv.getEnv "AWS_REGION"
    case fromText (Text.pack regionString)::(Either String Region) of
        Left x -> ioError (userError x)
        Right x -> return x
    
getRegionColor :: AlexaRequest -> Context () -> IO (Either AlexaResponse AlexaResponse)
getRegionColor r c = do
    result <- runExceptT $ eitherRegionColor (trace ("Received request " ++ show r) r) c
    case result of
        Left r -> return $ Right r
        Right r -> return $ Right r


eitherGeolocation :: AlexaRequest -> ExceptT AlexaResponse IO (Float, Float)
eitherGeolocation r = do
    let maybeGeolocation = fmap ((\c -> (latitudeInDegrees c, longitudeInDegrees c)) . coordinate) ((alexaGeolocation . context) r)
    case maybeGeolocation of
        Nothing -> createErrorResponse "Abilita servizi geografici"
        Just geolocation -> return geolocation


getCAPFromAlexaRequest :: AlexaRequest -> IO String
getCAPFromAlexaRequest r = runReq defaultHttpConfig $ do
    let _deviceId = pack $ (deviceId . device . alexaSystem . context) r
    let _consentToken = (consentToken . permissions . user . session) r
    r <- req
        GET
        (https "api.eu.amazonalexa.com" /: "v1" /: "devices" /: _deviceId /: "settings" /: "address" /: "countryAndPostalCode")
        NoReqBody
        jsonResponse
        (header "Authorization" (Char8.pack ("Bearer " ++ _consentToken)))
    let body = (responseBody r :: AlexaCAPResponse)
    return $ postalCode body

defaultCAP :: HttpException -> IO String
defaultCAP _ = return "0"

eitherItalianRegionByCAP :: AlexaRequest -> ExceptT AlexaResponse IO ItalianRegion
eitherItalianRegionByCAP r = do
    cap <- liftIO (catch (getCAPFromAlexaRequest r) defaultCAP)
    case regionByCap (read cap :: Int) of
        Nothing -> createErrorResponse "CAP non  va bene"
        Just region -> return region

eitherItalianRegionByGeo :: String -> (Float, Float) -> ExceptT AlexaResponse IO ItalianRegion
eitherItalianRegionByGeo geoServiceKey coords = do
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
    italianRegion <- (eitherItalianRegionByGeo geoServiceKey coordsFromRequest) <|> (eitherItalianRegionByCAP r)
    eitherRegionColorByRegion italianRegion


createColorResponse :: String -> ExceptT AlexaResponse IO AlexaResponse
createColorResponse color = ExceptT (pure resp)
    where resp = Right AlexaResponse {
        version = "1.0",
        response = AlexaResponsePayload {
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
    }

createErrorResponse :: String -> ExceptT AlexaResponse IO v
createErrorResponse message = ExceptT (pure resp)
    where resp = Left $ newResponseMessage message

newResponseMessage :: String -> AlexaResponse
newResponseMessage message = AlexaResponse {
    version = "1.0",
    response = AlexaResponsePayload {
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
}