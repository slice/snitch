{- | Datatypes and representations that have to do with builds of the Discord
 host and web application.
-}
module Snitch.Discord (
  -- * Branches
  Branch (..),
  branchBaseUrl,
  branchAppUrl,

  -- * Builds
  AppBuild (..),
  BuildId (..),
  BuildNumber (..),
  AppVersion (..),

  -- * Assets
  AppAssetType (..),
  AppAsset (..),
  assetTypeExtension,
  assetUrl,
) where

import Data.Hashable (Hashable (hashWithSalt))
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Req (Scheme (..), Url, https, (/:))

-- | A build ID (in practice, a textual hash). This works as a unique identifier for a build.
newtype BuildId = BuildId Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)

-- | A build number. This is seemingly incremented on every build, and works as a unique identifier for a build.
newtype BuildNumber = BuildNumber Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)

-- | Version information for a build of the web application.
data AppVersion = AppVersion
  { buildId :: BuildId
  -- ^ The ID of the build.
  , buildNumber :: BuildNumber
  -- ^ The number of the build.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)

-- | A build of the web application (loaded by the host).
data AppBuild = AppBuild
  { buildVersion :: AppVersion
  -- ^ The version of the build.
  , buildAssets :: [AppAsset]
  -- ^ The assets present in the build.
  }
  deriving stock (Show, Generic)

instance Eq AppBuild where
  a == b = buildId (buildVersion a) == buildId (buildVersion b)

instance Hashable AppBuild where
  hashWithSalt s (AppBuild{buildVersion = AppVersion{buildId}}) = s `hashWithSalt` buildId

-- | A kind of 'AppAsset'.
data AppAssetType
  = -- | A CSS stylesheet.
    Stylesheet
  | -- | A JavaScript file.
    Script
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

-- | An asset loaded by the web application.
data AppAsset = AppAsset
  { appAssetHash :: Text
  -- ^ The hash of the asset, identifying it on the CDN.
  , appAssetType :: AppAssetType
  -- ^ The type of the asset.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

-- | A build channel.
data Branch
  = -- | The default branch that the vast majority of Discord users are using.
    Stable
  | -- | The "public test build" branch. Nowadays, the app builds from this branch are identical to 'Canary'.
    PTB
  | -- | The "bleeding edge" branch. This branch receives updates frequently.
    Canary
  | -- | The development branch, mainly used by Discord employees.
    Development
  deriving (Eq, Show, Generic, Hashable)

{- | The base URL of the app for a certain branch. The 'Development' branch does not have one.

 >>> branchBase Stable
 Just (Url Https ("discord.com" :| []))
 >>> branchBase Development
 Nothing
-}
branchBaseUrl :: Branch -> Maybe (Url 'Https)
branchBaseUrl = \case
  Development -> Nothing
  Stable -> Just $ https "discord.com"
  PTB -> Just $ https "ptb.discord.com"
  Canary -> Just $ https "canary.discord.com"

-- | The URL to access the web application for a certain branch. The 'Development' branch does not have one.
branchAppUrl :: Branch -> Maybe (Url 'Https)
branchAppUrl = ((/: "app") <$>) . branchBaseUrl

-- | Returns the file extension used for a type of app asset.
assetTypeExtension :: AppAssetType -> Text
assetTypeExtension Stylesheet = "css"
assetTypeExtension Script = "js"

-- | Returns a URL that points to an 'AppAsset'.
assetUrl :: AppAsset -> Url 'Https
assetUrl (AppAsset{appAssetHash, appAssetType}) =
  https "discord.com" /: "assets" /: (appAssetHash <> "." <> assetTypeExtension appAssetType)
