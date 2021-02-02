module AWS.AWSTypes.AlexaFunctions.GetRegionColor(getRegionColor) where

import Control.Applicative((<|>))
import AWS.AWSTypes.AlexaMessages
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


data ResponseError = NoSuchCAPPermission | NoGeoServicePermission | 
                     RegionNotFound | InvalidCAP | ColorNotFoundForRegion |
                     RegionNotSpecified
                     deriving(Show, Eq)

error2msg :: [ResponseError] -> String
error2msg errors 
    | ColorNotFoundForRegion `elem` errors = "Non ho trovato il colore per la tua regione"
    | InvalidCAP `elem` errors = "CAP non valido"
    | RegionNotFound `elem` errors = "Non riesco a trovare la tua regione"
    | NoSuchCAPPermission `elem` errors = "Non posso accedere al CAP del tuo dispositivo"
    | NoGeoServicePermission `elem` errors = "Non posso accedere alla tua posizione geografica"
    | RegionNotSpecified `elem` errors = "Non ho capito la regione"
    | otherwise = "Errore sconosciuto"
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
        Left r -> return $ Right $ trace ("failed, response " ++ show r) $ newResponseMessage $ error2msg r
        Right r -> return $ Right $ trace ("success, response " ++ show r) r


eitherGeolocation :: AlexaRequest -> ExceptT [ResponseError] IO (Float, Float)
eitherGeolocation r = do
    let maybeGeolocation = fmap ((\c -> (latitudeInDegrees c, longitudeInDegrees c)) . coordinate) ((alexaGeolocation . context) r)
    case maybeGeolocation of
        Nothing -> createErrorResponse NoGeoServicePermission
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

eitherCAPFromAlexa :: AlexaRequest -> ExceptT [ResponseError] IO String
eitherCAPFromAlexa r = do
    maybeCAP <- liftIO (catch (getCAPFromAlexaRequest r) noCAP)
    case maybeCAP of
        Nothing -> createErrorResponse NoSuchCAPPermission
        Just cap -> return cap

eitherItalianRegionByCAP :: AlexaRequest -> ExceptT [ResponseError] IO ItalianRegion
eitherItalianRegionByCAP r = do
    cap <- eitherCAPFromAlexa r
    case regionByCap (read cap :: Int) of
        Just region -> return region
        _ -> createErrorResponse InvalidCAP

eitherItalianRegionByGeo :: AlexaRequest -> ExceptT [ResponseError] IO ItalianRegion
eitherItalianRegionByGeo r = do
    coords <- eitherGeolocation r
    awsRegion <- liftIO getAWSRegion    
    geoServiceKey <- liftIO $ runReaderT getGeoServiceKey awsRegion
    maybeItalianRegion <- liftIO $ runReaderT (runMaybeT (getRegion coords)) geoServiceKey
    case maybeItalianRegion of
        Nothing -> createErrorResponse RegionNotFound
        Just italianRegion -> return italianRegion


eitherHead :: [a] -> ResponseError -> ExceptT [ResponseError] IO a
eitherHead [] e = createErrorResponse e
eitherHead (v:_) _ = return v


maybeToEither :: Maybe a -> ResponseError -> ExceptT [ResponseError] IO a
maybeToEither Nothing e = createErrorResponse e
maybeToEither (Just x) _ = return x

eitherRegionColorByRegion :: ItalianRegion -> ExceptT [ResponseError] IO AlexaResponse
eitherRegionColorByRegion italianRegion = do
    maybeRegionColors <- liftIO $ runMaybeT getRegionColors
    case fmap (Map.lookup italianRegion) maybeRegionColors of
        Just (Just color) -> createColorResponse italianRegion color
        _ -> createErrorResponse ColorNotFoundForRegion


decodedItalianRegion :: String -> String
decodedItalianRegion "Friuli Venezia Giulia" = "Friuli"
decodedItalianRegion "Emilia Romagna" = "Emilia"
decodedItalianRegion "Romagna" = "Emilia"
decodedItalianRegion x = x

eitherItalianRegionByRequest :: AlexaRequest -> ExceptT [ResponseError] IO ItalianRegion
eitherItalianRegionByRequest r = do
    intent <- maybeToEither (_intent (request r)) RegionNotSpecified
    intentSlots <- maybeToEither (intentslots intent) RegionNotSpecified
    authorities <- resolutionsPerAuthority . slotresolutions <$> maybeToEither (Map.lookup "region" intentSlots) RegionNotSpecified
    authority <- eitherHead authorities RegionNotSpecified
    _value <- eitherHead (values authority) RegionNotSpecified
    let strItalianRegion = decodedItalianRegion $ (name . value) _value
    let italianRegion = read strItalianRegion :: ItalianRegion
    return italianRegion

eitherRegionColor :: AlexaRequest -> Context () -> ExceptT [ResponseError] IO AlexaResponse
eitherRegionColor r _ = do
    italianRegion <- eitherItalianRegionByRequest r <|> eitherItalianRegionByGeo r <|> eitherItalianRegionByCAP r
    eitherRegionColorByRegion italianRegion


createColorResponse :: ItalianRegion -> String -> ExceptT t IO AlexaResponse
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

createErrorResponse :: ResponseError -> ExceptT [ResponseError] IO v
createErrorResponse error = ExceptT (pure resp)
    where resp = Left [error]
