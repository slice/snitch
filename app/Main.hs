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
import Snitch.Scrape (ScrapeError, hitAppPage)

type Snitch sig m =
  ( Has Http sig m
  , Has (Throw ScrapeError) sig m
  , Has Trace sig m
  , Has (Exc.Lift IO) sig m
  , MonadIO m
  )

scrapeApp :: Snitch sig m => Branch -> m BuildId
scrapeApp branch = do
  trace $ "hitting app page for frontend " <> show branch
  (buildId, assets) <- hitAppPage branch
  trace $ "assets: " <> show assets
  pure buildId

main :: IO ()
main = throwLeft . runTrace . runThrow @ScrapeError . runHttpReq $ prettyTrace t
 where
  delaySeconds = threadDelay . (1000000 *)

  step :: Snitch sig m => m (Maybe BuildId)
  step = Just <$> (liftIO (delaySeconds 5) *> scrapeApp Canary)

  handleHttpException :: Snitch sig m => HttpException -> m ()
  handleHttpException exc = trace $ "Poll failed with HTTP exception: " ++ show exc

  safeStep :: Snitch sig m => m (Maybe BuildId)
  safeStep = Exc.catch step ((Nothing <$) . handleHttpException)

  safeCmp (Just a) (Just b) = a /= b
  safeCmp _ _ = False

  t :: Snitch sig m => m ()
  t = pollBy (Just $ BuildId "") safeStep (trace . show) safeCmp

  throwLeft :: Exception l => IO (Either l ()) -> IO ()
  throwLeft = (>>= either (void . throwIO) (const $ pure ()))
