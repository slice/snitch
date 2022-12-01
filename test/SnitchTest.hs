{-# OPTIONS_GHC -Wno-type-defaults -Wno-missing-signatures #-}

module Main (main) where

import Control.Exception (catch)
import Control.Exception.Base (SomeException)
import Control.Monad (void, when)
import Data.IORef
import Snitch
import Snitch.Discord
import Snitch.Parser
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec (parseMaybe)

counter :: IO (IORef Int, IO Int)
counter = boundedCounter maxBound

boundedCounter :: Int -> IO (IORef Int, IO Int)
boundedCounter n = do
  ref <- newIORef @Int 0
  pure (ref, tick ref)
 where
  tick ref = do
    value <- readIORef ref
    when (value + 1 > n) (fail "done")
    modifyIORef ref (+ 1)
    readIORef ref

main =
  defaultMain $
    testGroup
      "unit tests"
      [ testCase "polling" $ do
          -- Polling happens forever, so die after 4 generations.
          (tickerRef, ticker) <- boundedCounter 4
          (changedRef, changed) <- counter
          catch (poll 0 ticker (const . void $ changed)) (\(_ :: SomeException) -> pure ())

          ticks <- readIORef tickerRef
          ticks @?= 4
          changes <- readIORef changedRef
          changes @?= 4
      , testCase "parsing" $ do
          let fragment = "\"[BUILD INFO] Release Channel: \".concat(Ee,\", Build Number: \").concat(\"160745\""
          parseMaybe appBuildNumberParser fragment @?= Just (BuildNumber 160745)
      ]
