module Lib where

import GHC.Generics ()
import AWS.AWSTypes.AlexaMessages(AlexaRequest, AlexaResponse)
import AWS.AWSTypes.AlexaContext(AlexaContext)
import AWS.AWSFunctions(getRegionColor, debugMessage)
import Aws.Lambda ( Context )
import Data.Aeson
handler :: AlexaRequest -> Context () -> IO (Either String AlexaResponse)
handler = getRegionColor
-- handler :: Value -> Context () -> IO (Either String AlexaResponse)
-- handler = debugMessage
