module Lib where

import GHC.Generics ()
import Data.Aeson
import AWS.AWSTypes.AlexaMessages(AlexaRequest, AlexaResponse)
import AWS.AWSTypes.AlexaContext(AlexaContext)
import AWS.AWSFunctions(getRegionColor)
import Aws.Lambda ( Context )

handler :: AlexaRequest -> Context () -> IO (Either String AlexaResponse)
handler = getRegionColor
