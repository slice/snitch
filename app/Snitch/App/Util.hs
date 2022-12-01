module Snitch.App.Util where

import Control.Carrier.Interpret
import Control.Effect.Trace
import Control.Monad.IO.Class
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import System.Console.ANSI (ConsoleIntensity (BoldIntensity), SGR (..), hSetSGR)
import System.IO (hFlush, hPutStr, stderr)

-- | Injects a bolded timestamp before 'trace's.
prettyTrace ::
  (Has Trace sig m, MonadIO m) =>
  (forall s. Reifies s (Interpreter Trace m) => InterpretC s Trace m a) ->
  m a
prettyTrace = runInterpret $ \_ (Trace s) ctx -> do
  time <- liftIO getCurrentTime
  liftIO $ hSetSGR stderr [SetConsoleIntensity BoldIntensity]
  liftIO $ hFlush stderr
  let formatted = formatTime defaultTimeLocale "%F %T%04Q %EZ" time
  liftIO $ hPutStr stderr $ "[" <> formatted <> "] "
  liftIO $ hSetSGR stderr [Reset]
  liftIO $ hFlush stderr
  trace s
  pure ctx
