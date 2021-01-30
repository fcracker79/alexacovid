module AWS.AWSTypes.AlexaFunctions.EntryPoint where


import AWS.AWSTypes.AlexaMessages
    ( AlexaResponse,
      AlexaRequest(request),
      AlexaRequestPayload(_intent),
      AlexaIntent(intentname) )
import Aws.Lambda (Context)
import AWS.AWSTypes.AlexaFunctions.GetRegionColor (getRegionColor)
import Debug.Trace(trace)

entryPoint :: AlexaRequest -> Context () -> IO (Either String AlexaResponse)
entryPoint r c
    | maybeIntentName == Just "RegionColorIntent" = getRegionColor r c
    | maybeIntentName == Just "ArbitraryRegionColorIntent" = getRegionColor r c
    | otherwise = return $ trace "Unsupported intent" $ Left $ "Unsupported intent " ++ show maybeIntentName
    where maybeIntentName = fmap intentname ((_intent . request) r)