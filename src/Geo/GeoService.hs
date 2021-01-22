module Geo.GeoService where


{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

import GHC.Generics(Generic)
import Control.Monad.IO.Class(MonadIO, liftIO)
import Control.Monad(guard)
import Data.Aeson
import Control.Monad.Trans.Maybe(MaybeT)
import Control.Monad.Reader(ask)
import Control.Monad.Trans.Reader(ReaderT)
import Network.HTTP.Req (MonadHttp, jsonResponse, GET(..), runReq, req, https, (/:), (=:), defaultHttpConfig, responseBody, NoReqBody(..))
import qualified Data.HashMap.Lazy as M
import Data.Maybe(isJust, fromMaybe)
import Data.Text(Text)
import Regions(ItalianRegion(..))
import qualified Data.Map as Map
import Control.Applicative

type ApiKey = String


data GetRegionAddressComponent = GetRegionAddressComponent {
        long_name :: String,
        short_name:: String,
        types :: [String]
    } deriving(Generic, FromJSON, ToJSON, Eq, Show)

newtype GetRegionAddressResult = GetRegionAddressResult {
        address_components :: [GetRegionAddressComponent]
    } deriving(Generic, FromJSON, ToJSON, Eq, Show)

data GetRegionResponse = GetRegionResponse {
    status :: String,
    results :: [GetRegionAddressResult]
} deriving(Generic, FromJSON, ToJSON, Eq, Show)

getRegionHttpRequest :: Network.HTTP.Req.MonadHttp m => (Float, Float) -> ApiKey -> m [GetRegionAddressResult]
getRegionHttpRequest (lat, long) apiKey = do
    r <- req
            GET
            (https "maps.googleapis.com" /: "maps" /: "api" /: "geocode" /: "json")
            NoReqBody
            jsonResponse
            (("latlng" =: (show lat ++ "," ++ show long)) <> ("key" =: apiKey))
    let body = (responseBody r :: GetRegionResponse)
    (liftIO . print) body
    return $ if status body == "OK" then results body else []


transcodingMap :: Map.Map String ItalianRegion
transcodingMap = Map.fromList [
    ("Abruzzo", Abruzzo),
    ("Basilicata", Basilicata),
    ("Calabria", Calabria),
    ("Campania", Campania),
    ("Emilia-Romagna", Emilia),
    ("Friuli-Venezia Giulia", Friuli),
    ("Lazio", Lazio),
    ("Liguria", Liguria),
    ("Lombardy", Lombardia),
    ("Marche", Marche),
    ("Molise", Molise),
    ("Piemonte", Piemonte),
    ("Puglia", Puglia),
    ("Sardinia", Sardegna),
    ("Sicily", Sicilia),
    ("Tuscany", Toscana),
    ("Trentino-South-Tyrol", Trentino),
    ("Umbria", Umbria),
    ("Aosta", ValDAosta),
    ("Veneto", Veneto)]


getRegion :: MonadIO m => (Float, Float) -> MaybeT (ReaderT ApiKey m) ItalianRegion
getRegion pos = do
    apiKey <- ask
    resp <- runReq defaultHttpConfig $ getRegionHttpRequest pos apiKey
    (liftIO . print) resp
    case filter (elem "administrative_area_level_1" . types) [x | y <- resp, x <- address_components y] of
        [] -> empty
        (v:_) -> maybe empty return (Map.lookup (short_name v) transcodingMap)
