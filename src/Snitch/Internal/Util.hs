module Snitch.Internal.Util where

import Control.Arrow (left)
import Control.Effect.Throw
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')

maybeToRight :: l -> Maybe r -> Either l r
maybeToRight _ (Just a) = Right a
maybeToRight l Nothing = Left l

rightToMaybe :: Either l r -> Maybe r
rightToMaybe (Left _) = Nothing
rightToMaybe (Right r) = Just r

-- | Lifts a @'Maybe' a@ into @'Monad' a@ with effect @'Throw' e@.
liftMaybe :: (Has (Throw e) sig m) => e -> Maybe a -> m a
liftMaybe e = liftEither . maybeToRight e

lastMaybe :: [a] -> Maybe a
lastMaybe [a] = Just a
lastMaybe [] = Nothing
lastMaybe (_ : t) = lastMaybe t

-- | Decodes a 'ByteString', throwing an exception upon failure.
decodeUtf8Throwing :: (Has (Throw e) sig m) => e -> ByteString -> m Text
decodeUtf8Throwing e = liftEither . left (const e) . decodeUtf8'
