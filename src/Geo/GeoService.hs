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
import Data.Maybe(isJust)
import Data.Text(Text)
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


getRegion :: MonadIO m => (Float, Float) -> MaybeT (ReaderT ApiKey m) String
getRegion pos = do
    apiKey <- ask
    resp <- runReq defaultHttpConfig $ getRegionHttpRequest pos apiKey
    (liftIO . print) resp
    case filter (elem "administrative_area_level_1" . types) [x | y <- resp, x <- address_components y] of
        [] -> return mempty
        (v:_) -> return $ short_name v
