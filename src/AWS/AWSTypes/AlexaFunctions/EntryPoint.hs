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
    | otherwise = return $ trace unsupportedIntentMessage $ Left unsupportedIntentMessage
    where maybeIntentName = fmap intentname ((_intent . request) r)
          unsupportedIntentMessage = "Unsupported intent " ++ show maybeIntentName ++ ", message " ++ show r