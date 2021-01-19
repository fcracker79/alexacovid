module Geo.GeoService where


{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class(MonadIO, liftIO)
import Control.Monad(guard)
import Data.Aeson
import Control.Monad.Trans.Maybe(MaybeT)
import Control.Monad.Reader(ask)
import Control.Monad.Trans.Reader(ReaderT)
import Network.HTTP.Req (jsonResponse, GET(..), runReq, req, https, (/:), (=:), defaultHttpConfig, responseBody)
import Data.Map.Lazy(lookup)
import Data.Maybe(isJust)

type ApiKey = String


getRegion :: MonadIO m => (Float, Float) -> MaybeT (ReaderT ApiKey m) String
getRegion (lat, long) = do
    apiKey <- ask
    resp <- runReq defaultHttpConfig $ do
            r <- req
                        GET
                        (https "maps.googleapis.com" /: "maps" /: "api" /: "geocode" /: "json")
                        mempty
                        jsonResponse
                        ("latlng" =: (show lat ++ "," ++ show long))
            let Object body = (responseBody r :: Value)
            (liftIO . print) body
            guard $ isJust (Data.Map.Lazy.lookup "status" body)
            return body
    (liftIO . print) resp
    return mempty