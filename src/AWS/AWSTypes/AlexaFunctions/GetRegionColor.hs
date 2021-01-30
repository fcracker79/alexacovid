module AWS.AWSTypes.AlexaFunctions.GetRegionColor(getRegionColor) where

import Control.Applicative((<|>))
import AWS.AWSTypes.AlexaMessages
    ( AlexaResponsePayload(AlexaResponsePayload, outputSpeech, card,
                           reprompt, directives, shouldEndSession),
      AlexaResponse(..),
      AlexaOutputSpeech(AlexaOutputSpeech, aostype, aostext,
                        aosplayBehavior),
      AlexaCard(AlexaCard, ctype, ctitle, ctext),
      AlexaRequest(context, session),
      newResponseMessage )
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


newtype AlexaCAPResponse = AlexaCAPResponse {
    postalCode :: String
} deriving(Show, Eq, Generic, FromJSON, ToJSON)

getAWSRegion :: IO Region
getAWSRegion = do
    regionString <- Sysenv.getEnv "AWS_REGION"
    case fromText (Text.pack regionString)::(Either String Region) of
        Left x -> ioError (userError x)
        Right x -> return x
    
getRegionColor :: AlexaRequest -> Context () -> IO (Either String AlexaResponse)
getRegionColor r c = do
    result <- runExceptT $ eitherRegionColor (trace ("Received request " ++ show r) r) c
    case result of
        Left r -> return $ Right $ trace ("failed, response " ++ show r) r
        Right r -> return $ Right $ trace ("success, response " ++ show r) r


eitherGeolocation :: AlexaRequest -> ExceptT AlexaResponse IO (Float, Float)
eitherGeolocation r = do
    let maybeGeolocation = fmap ((\c -> (latitudeInDegrees c, longitudeInDegrees c)) . coordinate) ((alexaGeolocation . context) r)
    case maybeGeolocation of
        Nothing -> createErrorResponse "Abilita la skill InfoCovid ad accedere al servizio di geolocalizzazione"
        Just geolocation -> return geolocation


getCAPFromAlexaRequest :: AlexaRequest -> IO (Maybe String)
getCAPFromAlexaRequest r = runReq defaultHttpConfig $ do
    let _deviceId = pack $ (deviceId . device . alexaSystem . context) r
    let _maybeConsentToken = (consentToken . permissions . user . session) r
    case _maybeConsentToken of
        Nothing -> return Nothing
        Just _consentToken -> do
            r <- req
                GET
                (https "api.eu.amazonalexa.com" /: "v1" /: "devices" /: _deviceId /: "settings" /: "address" /: "countryAndPostalCode")
                NoReqBody
                jsonResponse
                (header "Authorization" (Char8.pack ("Bearer " ++ _consentToken)))
            let body = (responseBody r :: AlexaCAPResponse)
            return $ Just (postalCode body)

noCAP :: HttpException -> IO (Maybe String)
noCAP _ = return Nothing

eitherCAPFromAlexa :: AlexaRequest -> ExceptT AlexaResponse IO String
eitherCAPFromAlexa r = do
    maybeCAP <- liftIO (catch (getCAPFromAlexaRequest r) noCAP)
    case maybeCAP of
        Nothing -> createErrorResponse "Abilita la skill InfoCovid ad accedere al CAP del tuo dispositivo Alexa"
        Just cap -> return cap

eitherItalianRegionByCAP :: AlexaRequest -> ExceptT AlexaResponse IO ItalianRegion
eitherItalianRegionByCAP r = do
    cap <- eitherCAPFromAlexa r
    case regionByCap (read cap :: Int) of
        Just region -> return region
        _ -> createErrorResponse $ "Il CAP " ++ cap ++ " specificato nella configurazione di Alexa non è valido"

eitherItalianRegionByGeo :: AlexaRequest -> ExceptT AlexaResponse IO ItalianRegion
eitherItalianRegionByGeo r = do
    coords <- eitherGeolocation r
    awsRegion <- liftIO getAWSRegion    
    geoServiceKey <- liftIO $ runReaderT getGeoServiceKey awsRegion
    maybeItalianRegion <- liftIO $ runReaderT (runMaybeT (getRegion coords)) geoServiceKey
    case maybeItalianRegion of
        Nothing -> createErrorResponse "Non ho trovato la tua regione" 
        Just italianRegion -> return italianRegion


eitherRegionColorByRegion :: ItalianRegion -> ExceptT AlexaResponse IO AlexaResponse
eitherRegionColorByRegion italianRegion = do
    maybeRegionColors <- liftIO $ runMaybeT getRegionColors
    case fmap (Map.lookup italianRegion) maybeRegionColors of
        Just (Just color) -> createColorResponse italianRegion color
        _ -> createErrorResponse "Non ho trovato il colore per la tua regione" 

eitherRegionColor :: AlexaRequest -> Context () -> ExceptT AlexaResponse IO AlexaResponse
eitherRegionColor r _ = do
    italianRegion <- eitherItalianRegionByGeo r <|> eitherItalianRegionByCAP r
    eitherRegionColorByRegion italianRegion


createColorResponse :: ItalianRegion -> String -> ExceptT AlexaResponse IO AlexaResponse
createColorResponse italianRegion color = ExceptT (pure resp)
    where resp = Right AlexaResponse {
        version = "1.0",
        response = AlexaResponsePayload {
            outputSpeech = AlexaOutputSpeech {
                aostype = "PlainText",
                aostext = "Oggi il colore della regione " ++ show italianRegion ++ "è " ++ color,
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
