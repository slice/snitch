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

data SnitchError = MissingBuildNumber
  deriving stock (Show, Eq)

instance Exception SnitchError

scrapeApp :: Snitch sig m => Branch -> m FrontalAppBuildInfo
scrapeApp branch = do
  trace $ "hitting app page for frontend " <> show branch
  bundle@(_, assets) <- hitAppPage branch
  trace $ "assets: " <> show assets
  pure bundle

newBuildIdDetected :: Snitch sig m => Poll -> m ()
newBuildIdDetected Starting = pure ()
newBuildIdDetected (Ok (_, assets)) = do
  trace "new build id detected!"
  let entrypoint = entrypointScript . scripts $ assets
  trace $ "entrypoint script: " <> show entrypoint
  entrypointText <- getDecoding DecodingFailed $ assetUrl entrypoint
  buildNumber <- liftMaybe MissingBuildNumber $ siftBuildNumber (Js entrypointText)
  trace $ "build number: " <> show buildNumber

data Poll = Ok FrontalAppBuildInfo | Starting
  deriving stock (Show, Eq)

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

  step :: Snitch sig m => m Poll
  step = liftIO (delaySeconds 5) *> (Ok <$> scrapeApp Canary)

  tryPoll :: Snitch sig m => m a -> m (Maybe a)
  tryPoll f = Exc.catch (Just <$> f) ((Nothing <$) . handleHttpException)

  handleHttpException :: Snitch sig m => HttpException -> m ()
  handleHttpException exc = trace $ "Poll failed with HTTP exception: " ++ show exc

  safeCmp (Just a) (Just b) = a /= b
  safeCmp _ _ = False

  app :: Snitch sig m => m ()
  app = pollBy @_ @(Maybe Poll) (Just Starting) (tryPoll step) (maybe (pure ()) newBuildIdDetected) safeCmp

  throwLeft :: (MonadIO m, Exception l) => m (Either l ()) -> m ()
  throwLeft = (>>= either (void . liftIO . throwIO) (const $ pure ()))
