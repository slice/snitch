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
  appAssetType,
  AppAssets (..),
  assetTypeExtension,
  assetUrl,
  assetUrl',
) where

import Data.Hashable (Hashable (hashWithSalt))
import Data.List.NonEmpty (NonEmpty)
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
  , buildAssets :: AppAssets
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

type ScriptAsset = AppAsset 'Script
type StylesheetAsset = AppAsset 'Stylesheet

-- | An asset loaded by the web application.
data AppAsset (t :: AppAssetType) where
  MkScript :: Text -> ScriptAsset
  MkStylesheet :: Text -> StylesheetAsset

deriving instance Eq (AppAsset t)
deriving instance Show (AppAsset t)

instance Hashable (AppAsset t) where
  hashWithSalt s (MkScript hash) = s `hashWithSalt` hash
  hashWithSalt s (MkStylesheet hash) = s `hashWithSalt` hash

-- | Returns an asset's type.
appAssetType :: AppAsset t -> AppAssetType
appAssetType (MkScript _) = Script
appAssetType (MkStylesheet _) = Stylesheet

-- | A set of assets associated with a build of the web application.
data AppAssets = AppAssets
  { scripts :: NonEmpty (AppAsset 'Script)
  -- ^ The scripts present in the build.
  , stylesheets :: NonEmpty (AppAsset 'Stylesheet)
  -- ^ The stylesheets present in the build.
  }
  deriving stock (Show, Eq, Generic)

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

-- | A helper function to return the file extension for an asset.
extension :: AppAsset t -> Text
extension = assetTypeExtension . appAssetType

-- | Returns a URL that points to an 'AppAsset'.
assetUrl :: AppAsset t -> Url 'Https
assetUrl asset@(MkScript hash) = assetUrl' hash (extension asset)
assetUrl asset@(MkStylesheet hash) = assetUrl' hash (extension asset)

-- | Constructs a URL that points to an asset on Discord's CDN.
assetUrl' ::
  -- | The hash of the asset.
  Text ->
  -- | The file extension of the asset.
  Text ->
  Url 'Https
assetUrl' hash ext = https "discord.com" /: "assets" /: (hash <> "." <> ext)
