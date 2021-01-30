module Lib where

import AWS.AWSTypes.AlexaMessages(AlexaRequest, AlexaResponse)
import AWS.AWSTypes.AlexaContext(AlexaContext)
import AWS.AWSTypes.AlexaFunctions.GetRegionColor(getRegionColor)
import AWS.AWSTypes.AlexaFunctions.Debug(debugMessage)
import Aws.Lambda ( Context )
import AWS.AWSTypes.AlexaFunctions.EntryPoint (entryPoint)
import Data.Aeson

handler :: AlexaRequest -> Context () -> IO (Either String AlexaResponse)
handler = entryPoint
-- handler :: Value -> Context () -> IO (Either String AlexaResponse)
-- handler = debugMessage
