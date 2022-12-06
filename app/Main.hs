module Main where

import Control.Algebra
import Control.Carrier.Throw.Either (runThrow)
import Control.Carrier.Trace.Printing
import Control.Concurrent (threadDelay)
import Control.Effect.Exception qualified as Exc
import Control.Effect.Throw
import Control.Exception (Exception, throwIO)
import Control.Monad (void)
import Control.Monad.IO.Class
import Network.HTTP.Req (HttpException)
import Snitch
import Snitch.App.Util (prettyTrace)
import Snitch.Discord
import Snitch.Effects
import Snitch.Internal.Util
import Snitch.Scrape

type Snitch sig m =
  ( Has Http sig m
  , Has (Throw ScrapeError) sig m
  , Has (Throw SnitchError) sig m
  , Has Trace sig m
  , Has (Exc.Lift IO) sig m
  , MonadIO m
  )

data SnitchError = MissingEntrypointScript | MissingBuildNumber
  deriving stock (Show, Eq)

instance Exception SnitchError

scrapeApp :: Snitch sig m => Branch -> m (BuildId, [AppAsset])
scrapeApp branch = do
  trace $ "hitting app page for frontend " <> show branch
  bundle@(_, assets) <- hitAppPage branch
  trace $ "assets: " <> show assets
  pure bundle

newBuildIdDetected :: Snitch sig m => FrontalAppBuildInfo -> m ()
newBuildIdDetected (_, assets) = do
  trace "new build id detected!"
  -- TODO: This error should technically be impossible. Make this
  -- unrepresentable in the future with non-empty lists.
  entrypoint <- liftMaybe MissingEntrypointScript $ entrypointScript assets
  trace $ "entrypoint script: " <> show entrypoint
  entrypointText <- getDecoding DecodingFailed $ assetUrl entrypoint
  buildNumber <- liftMaybe MissingBuildNumber $ siftBuildNumber (Js entrypointText)
  trace $ "build number: " <> show buildNumber

main :: IO ()
main =
  runTrace
    . throwLeft
    . runThrow @SnitchError
    . throwLeft
    . runThrow @ScrapeError
    . runHttpReq
    $ prettyTrace app
 where
  delaySeconds = threadDelay . (1000000 *)

  step :: Snitch sig m => m FrontalAppBuildInfo
  step = liftIO (delaySeconds 5) *> scrapeApp Canary

  handleHttpException :: Snitch sig m => HttpException -> m ()
  handleHttpException exc = trace $ "Poll failed with HTTP exception: " ++ show exc

  safeStep :: Snitch sig m => m (Maybe FrontalAppBuildInfo)
  safeStep = Exc.catch (Just <$> step) ((Nothing <$) . handleHttpException)

  safeCmp (Just a) (Just b) = a /= b
  safeCmp _ _ = False

  app :: Snitch sig m => m ()
  app = pollBy (Just (BuildId "", [])) safeStep (maybe (pure ()) newBuildIdDetected) safeCmp

  throwLeft :: (MonadIO m, Exception l) => m (Either l ()) -> m ()
  throwLeft = (>>= either (void . liftIO . throwIO) (const $ pure ()))
