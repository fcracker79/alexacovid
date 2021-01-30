module RegionColors where

import Data.Text.Encoding (encodeUtf8,  decodeUtf8 )
import Text.HTML.Scalpel ( scrapeURL, attr, chroots, anySelector )
import Control.Monad.Trans.Maybe(MaybeT(..))
import Control.Monad.IO.Class(MonadIO, liftIO)
import Control.Applicative(Alternative((<|>)), empty)
import Control.Monad (guard)
import Data.Map.Strict(fromList, Map)
import Data.Map (compose)
import Regions(ItalianRegion(..))
import Data.Aeson(encode, decode)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Control.Monad.IO.Unlift ( MonadIO(liftIO) )
import Network.AWS.DynamoDB
    ( getItem,
      giKey,
      girsItem,
      piItem,
      putItem,
      attributeValue,
      avS,
      AttributeValue )
import qualified System.Environment as Sysenv
import Network.AWS.Data (fromText)
import Control.Monad.Trans.AWS
    ( runAWST,
      send,
      newEnv,
      within,
      newLogger,
      runResourceT,
      Credentials(Discover),
      HasEnv(envLogger),
      LogLevel(Debug),
      Region )
import Control.Lens ((?~), (^.), (&), (.~), (<&>))
import Control.Lens.Combinators (set)
import System.IO (stdout)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import qualified Data.HashMap.Strict as HashMap
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.HashMap.Strict
import Control.Error.Util ( hoistMaybe )


transcodingMap :: Map ItalianRegion String
transcodingMap = fromList [
    (Abruzzo, "Abruzzo"),
    (Basilicata, "Basilicata"),
    (Calabria, "Calabria"),
    (Campania, "Campania"),
    (Emilia,"Emilia-Romagna"),
    (Friuli,"Friuli-Venezia Giulia"),
    (Lazio, "Lazio"),
    (Liguria, "Liguria"),
    (Lombardia, "Lombardia"),
    (Marche, "Marche"),
    (Molise, "Molise"),
    (Piemonte, "Piemonte"),
    (Puglia, "Puglia"),
    (Sardegna, "Sardegna"),
    (Sicilia, "Sicilia"),
    (Toscana, "Toscana"),
    (Trentino, "Trentino Alto Adige (P.A. Bolzano)"),
--    ("Trentino Alto Adige (P.A. Trento)","giallo"),
    (Umbria,"Umbria"),
    (ValDAosta, "Valle d'Aosta"),
    (Veneto,"Veneto")]

getRegionColorsListFromWebsite :: MaybeT IO [(String, String)]
getRegionColorsListFromWebsite = MaybeT $ do
                               scrapeURL "https://covidzone.info" scraper
                               where scraper = chroots "path" $ do
                                                                d <- attr "d" anySelector
                                                                region  <- attr "title" anySelector
                                                                guard (region /= "")
                                                                color <- attr "color" anySelector
                                                                guard (color /= "")
                                                                return (region, color)

getRegionColorsMapFromWebsite :: MaybeT IO (Map String String)
getRegionColorsMapFromWebsite = fromList <$> getRegionColorsListFromWebsite


getRegionColors :: MaybeT IO (Map ItalianRegion String)
getRegionColors = do
    regionColors <- getRegionColorsFromCache <|> do
                                                 liftIO $ print "getRegionColorsMapFromWebsite"
                                                 colorsFromWebsite <- getRegionColorsMapFromWebsite
                                                 liftIO $ print ("getRegionColorsMapFromWebsite, saving cache" ++ show colorsFromWebsite)
                                                 saveRegionColorsToCache colorsFromWebsite
                                                 return colorsFromWebsite
    return $ compose regionColors transcodingMap 


getAWSRegion :: IO Region
getAWSRegion = do
    regionString <- Sysenv.getEnv "AWS_REGION"
    case fromText (Text.pack regionString)::(Either String Region) of
        Left x -> ioError (userError x)
        Right x -> return x

getDynamoDBTableName :: IO Text.Text
getDynamoDBTableName = Text.pack <$> Sysenv.getEnv "DYNAMODB_CACHE_TABLE_NAME"

attributeKey :: AttributeValue
attributeKey = attributeValue & (avS ?~ "regionColorsCache")


saveRegionColorsToCache :: Map String String -> MaybeT IO ()
saveRegionColorsToCache regionColors = do
    liftIO $ print "saveRegionColorsToCache"
    let dataToSave = decodeUtf8 $ toStrict $ encode regionColors
    liftIO $ print $ "saveRegionColorsToCache, data to save " ++ show dataToSave
    lgr <- newLogger Debug stdout
    env <- newEnv Discover <&> set envLogger lgr
    region <- liftIO getAWSRegion
    tableName <- liftIO getDynamoDBTableName
    liftIO $ print $ "saveRegionColorsToCache, table " ++ show tableName
    nowFloat <- liftIO getPOSIXTime
    liftIO $ print $ "saveRegionColorsToCache, nowFloat " ++ show nowFloat
    let key = attributeKey
    let jsonRegionColors = attributeValue & (avS ?~ dataToSave)
    liftIO $ print $ "saveRegionColorsToCache, jsonRegionColors " ++ show jsonRegionColors
    let now = attributeValue & (avS ?~ Text.pack (show nowFloat))
    liftIO $ print $ "saveRegionColorsToCache, now " ++ show now
    let timestampedRegionColors = 
            HashMap.fromList [
                ("id", key), 
                ("timestamp", now), 
                ("regionColors", jsonRegionColors)
            ]
    liftIO $ print ("saveRegionColorsToCache, item " ++ show timestampedRegionColors)
    liftIO . runResourceT . runAWST env . within region $ do
        send $ putItem tableName & piItem .~ timestampedRegionColors
    liftIO $ print "saveRegionColorsToCache complete "
    return ()


getRegionColorsFromCache :: MaybeT IO (Map String String)
getRegionColorsFromCache = do
    liftIO $ print "getRegionColorsFromCache"
    lgr <- newLogger Debug stdout
    env <- newEnv Discover <&> set envLogger lgr
    region <- liftIO getAWSRegion
    tableName <- liftIO getDynamoDBTableName
    item <- liftIO . runResourceT . runAWST env . within region $ do
        send $ getItem tableName & giKey .~ HashMap.fromList [("id", attributeKey)]
    liftIO $ print ("getRegionColorsFromCache: item " ++ show item)
    let rowItem  = item ^. girsItem
    liftIO $ print ("getRegionColorsFromCache: rowItem " ++ show item)
    timestampAttribute <- hoistMaybe $ Data.HashMap.Strict.lookup "timestamp" rowItem
    liftIO $ print ("getRegionColorsFromCache: timestampAttribute " ++ show timestampAttribute)
    timestamp <- Text.unpack <$> hoistMaybe (timestampAttribute ^. avS)
    liftIO $ print ("getRegionColorsFromCache: timestamp " ++ show timestamp)
    now <- liftIO getPOSIXTime
    guard ((read timestamp :: POSIXTime) < now + 2 * 3600)
    regionColorsAttribute <- hoistMaybe $ Data.HashMap.Strict.lookup "regionColors" rowItem
    jsonRegionColors <- fromStrict . encodeUtf8 <$> hoistMaybe (regionColorsAttribute ^. avS)
    liftIO $ print ("getRegionColorsFromCache: returning " ++ show jsonRegionColors)
    hoistMaybe (decode jsonRegionColors :: Maybe (Map String String))
