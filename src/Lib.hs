module Lib where

import GHC.Generics ()
import Data.Aeson
import AWS.AlexaMessages(AlexaRequest, AlexaResponse)
import AWS.AWSFunctions(getRegionColor)
import Aws.Lambda ( Context )

handler :: AlexaRequest -> Context () -> IO (Either String AlexaResponse)
handler = getRegionColor
