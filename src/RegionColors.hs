module RegionColors (getRegionColors) where

import Data.Text.Encoding ( decodeUtf8 )
import Text.HTML.Scalpel ( scrapeURL, attr, chroots, anySelector )
import Control.Monad.Trans.Maybe(MaybeT(..))
import Control.Monad.IO.Class(MonadIO, liftIO)
import Control.Applicative(empty)
import Control.Monad (guard)
import Data.Map.Strict(fromList, Map)
import Data.Map (compose)
import Regions(ItalianRegion(..))
import Data.Aeson(encode, decode)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Control.Monad.IO.Unlift
import Network.AWS.DynamoDB
import qualified System.Environment as Sysenv
import Network.AWS.Data (fromText)
import Control.Monad.Trans.AWS
import Control.Lens ((?~), (^.), (&), (.~), (<&>))
import Control.Lens.Combinators (set)
import System.IO (stdout)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import qualified Data.HashMap.Strict as HashMap
import Data.ByteString.Lazy (toStrict)
import qualified Data.HashMap.Strict


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

getRegionColorsList :: MaybeT IO [(String, String)]
getRegionColorsList = MaybeT $ do
                               scrapeURL "https://covidzone.info" scraper
                               where scraper = chroots "path" $ do
                                                                d <- attr "d" anySelector
                                                                region  <- attr "title" anySelector
                                                                guard (region /= "")
                                                                color <- attr "color" anySelector
                                                                guard (color /= "")
                                                                return (region, color)

getRegionColosMap :: MaybeT IO (Map String String)
getRegionColosMap = fromList <$> getRegionColorsList


getRegionColors :: MaybeT IO (Map ItalianRegion String)
getRegionColors = flip compose transcodingMap . fromList <$> getRegionColorsList


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

saveRegionColorsToCache :: Map String String -> IO ()
saveRegionColorsToCache regionColors = do
    let dataToSave = decodeUtf8 $ toStrict $ encode regionColors
    lgr <- newLogger Debug stdout
    env <- newEnv Discover <&> set envLogger lgr
    region <- getAWSRegion
    tableName <- getDynamoDBTableName
    nowFloat <- getPOSIXTime
    let key = attributeKey
    let jsonRegionColors = attributeValue & (avS ?~ dataToSave)
    let now = attributeValue & (avS ?~ Text.pack (show now))
    let timestampedRegionColors = 
            HashMap.fromList [
                ("id", key), 
                ("timestamp", now), 
                ("regionColors", jsonRegionColors)
            ]
    runResourceT . runAWST env . within region $ do
        send $ putItem tableName & piItem .~ timestampedRegionColors
    return ()


getRegionColorsFromCache :: MaybeT IO (Map String String)
getRegionColorsFromCache = do
    lgr <- newLogger Debug stdout
    env <- newEnv Discover <&> set envLogger lgr
    region <- liftIO getAWSRegion
    tableName <- liftIO getDynamoDBTableName
    item <- liftIO . runResourceT . runAWST env . within region $ do
        send $ getItem tableName & giKey .~ HashMap.fromList [("id", attributeKey)]
    let rowItem  = item ^. girsItem
    timestampAttribute <- MaybeT $ pure $ Data.HashMap.Strict.lookup "timestamp" rowItem
    timestamp <- MaybeT $ pure $ timestampAttribute ^. avS
    let x = timestamp + 1
    MaybeT $ pure Nothing
