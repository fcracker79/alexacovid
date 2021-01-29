module AWS.AWSTypes.AlexaSession where


import Data.Aeson ( FromJSON, ToJSON )
import GHC.Generics(Generic)
import Data.Map(Map)

newtype PermissionScope = PermissionScope {
    status :: String
} deriving(Show, Eq, Generic, FromJSON, ToJSON)

type PermissionScopes = Map String PermissionScope

data Permissions = Permissions {
    scopes :: PermissionScopes,
    consentToken :: Maybe String
} deriving(Show, Eq, Generic, FromJSON, ToJSON)

data User = User {
    userId :: String,
    permissions :: Permissions
} deriving(Show, Eq, Generic, FromJSON, ToJSON)

newtype Application = Application {
    applicationId :: String
} deriving(Show, Eq, Generic, FromJSON, ToJSON)

data AlexaSession = AlexaSession {
    sessionId :: String,
    application :: Application,
    user :: User
} deriving(Show, Eq, Generic, FromJSON, ToJSON)