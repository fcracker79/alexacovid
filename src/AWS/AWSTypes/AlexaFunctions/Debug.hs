module AWS.AWSTypes.AlexaFunctions.Debug(debugMessage) where

import AWS.AWSTypes.AlexaMessages(AlexaResponse)
import Aws.Lambda ( Context(..) )
import Data.Aeson(Value, encode)

debugMessage :: Value -> Context () -> IO (Either String AlexaResponse)
debugMessage r c = do
    print $ "THIS IS MY ENTIRE MESSAGE " ++ (show . encode) r
    return $ Left "DEBUG MESSAGE"