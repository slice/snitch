module Snitch.Scrape (
  -- * Types
  Html (..),
  Js (..),
  FrontalAppBuildInfo,

  -- * Errors
  ScrapeError (..),

  -- * Scraping
  lookupBuildId,
  hitAppPage,
  entrypointScript,
  parseAssets,
  siftBuildNumber,
) where

import Control.Effect.Throw
import Control.Exception (Exception)
import Control.Monad (unless, (<=<))
import Data.ByteString (ByteString)
import Data.List (tails)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Snitch.Discord
import Snitch.Effects (Http, Response (..), get)
import Snitch.Internal.Util
import Snitch.Parser
import Text.HTML.TagSoup (Tag (..), parseTags)
import Text.Megaparsec qualified

-- | A newtype for HTML text.
newtype Html = Html {unHtml :: Text}

-- | A newtype for JavaScript source code.
newtype Js = Js {unJs :: Text}

-- | Errors thrown when scraping.
data ScrapeError
  = -- | The specified 'Branch' has no application page.
    BranchHasNoApp
  | -- | Failed to decode text into UTF-8.
    DecodingFailed
  | -- | Information expected to be present was missing.
    MissingInformation Text
  deriving stock (Eq, Show)

instance Exception ScrapeError

extractHash :: Text -> Maybe Text
extractHash text = do
  components <- T.split (== '.') <$> T.stripPrefix "/assets/" text
  -- Remove the file extension, without clobbering any other periods in the
  -- filename.
  pure $ T.intercalate "." $ take (length components - 1) components

tryDecode :: (Has (Throw ScrapeError) sig m) => ByteString -> m Text
tryDecode = decodeUtf8Throwing DecodingFailed

-- | Tries to look up a 'BuildId' returned from a 'Response'.
lookupBuildId :: Response -> Maybe BuildId
lookupBuildId r = do
  headerBs <- responseLookupHeader r (encodeUtf8 "x-build-id")
  (BuildId <$>) . rightToMaybe . decodeUtf8' $ headerBs

{- | The identifying information of an app build that can be scraped with only
 a single request.
-}
type FrontalAppBuildInfo = (BuildId, [AppAsset])

{- | Requests a branch's app page, extracting the 'BuildId' from the response
 headers and list of 'AppAsset's from the page HTML.

 'MissingInformation' is thrown if there is a missing or malformed @x-build-id@
 header, or if there aren't any scripts or stylesheets in the HTML.
-}
hitAppPage :: (Has Http sig m, Has (Throw ScrapeError) sig m) => Branch -> m FrontalAppBuildInfo
hitAppPage branch = do
  appUrl <- liftEither $ maybe (Left BranchHasNoApp) Right $ branchAppUrl branch
  response <- get appUrl
  html <- (Html <$>) . tryDecode . responseBody $ response
  buildId <- liftMaybe (MissingInformation "Missing or malformed x-build-id header") $ lookupBuildId response
  let assets = parseAssets html
  unless (any ((Script ==) . appAssetType) assets) $ throwError $ MissingInformation "No scripts found"
  unless (any ((Stylesheet ==) . appAssetType) assets) $ throwError $ MissingInformation "No stylesheets found"
  pure (buildId, assets)

-- | Sifts a build number from the content of the entire entrypoint script.
siftBuildNumber :: Js -> Maybe BuildNumber
siftBuildNumber (Js text) = do
  let (_, remainder) = T.breakOn buildInfoHeading text
  -- Reinject the double quote that begins the string containing the heading,
  -- because the parser is expecting it.
  let truncatedFragment = T.take 256 $ "\"" <> remainder
  parseMaybeLenient appBuildNumberParser truncatedFragment
 where
  parseMaybeLenient p s = rightToMaybe $ Text.Megaparsec.parse p "entrypoint script" s

{- | Finds the "entrypoint" script from a list of 'AppAsset's.

 Historically, it has been (and currently is) the last script in the HTML, but
 this may change in the future, and we should not rely on this being the case.
 However, it is much cheaper to assume that the last script is the entrypoint.
-}
entrypointScript :: [AppAsset] -> Maybe AppAsset
entrypointScript = lastMaybe . filter ((Script ==) . appAssetType)

-- | Parses the appropriate 'HTML' tags into 'AppAsset's.
parseAssets :: Html -> [AppAsset]
parseAssets (Html html) =
  scripts ++ stylesheets
 where
  tags = parseTags html
  stylesheets =
    [ AppAsset href Stylesheet
    | TagOpen "link" attrs@(extractHash <=< lookup "href" -> Just href) : _ <- tails tags
    , lookup "rel" attrs == Just "stylesheet"
    ]
  scripts =
    [ AppAsset src Script
    | TagOpen "script" (extractHash <=< lookup "src" -> Just src) : _ <- tails tags
    ]
