module Geo.GeoCredentials where

import Network.AWS.SSM.GetParameter
    ( gWithDecryption, getParameter, gprsParameter )
import Network.AWS.SSM.Types ( pValue )
import Network.AWS
    ( newEnv,
      newLogger,
      runResourceT,
      Credentials(Discover),
      HasEnv(envLogger),
      LogLevel(Debug) )
import Control.Monad.Trans.AWS
    (Region,  runAWST,
      send,
      newEnv,
      within,
      newLogger,
      runResourceT,
      Credentials(Discover),
      HasEnv(envLogger),
      LogLevel(Debug) )
import Control.Monad.Reader(ask)
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Trans.Reader(ReaderT)
import System.Environment as Sysenv ( getEnv )
import System.IO ( stdout )
import Control.Lens ( (&), (<&>), (^.), (?~), set )
import qualified Data.Text as Text

getGeoServiceKey :: ReaderT Region IO String
getGeoServiceKey = do
    region <- ask
    lgr <- newLogger Debug stdout
    env <- newEnv Discover <&> set envLogger lgr
    recaptchaSecretKey <- liftIO (Text.pack <$> Sysenv.getEnv "RECAPTCHA_SSM_PARAMETER_NAME")
    response <- runResourceT . runAWST env . Control.Monad.Trans.AWS.within region $ do
        Control.Monad.Trans.AWS.send $ getParameter recaptchaSecretKey & gWithDecryption  ?~ True

    let maybeParam = response ^. gprsParameter
    case maybeParam of
        Nothing -> liftIO $ ioError (userError "SSM Parameter not found")
        Just parameter -> do
            let parameterValue = parameter ^. pValue
            case parameterValue of
                Nothing -> liftIO $ ioError (userError "Missing SSM Parameter Value")
                Just v -> return $ Text.unpack v
