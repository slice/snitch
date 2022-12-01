module Snitch.Internal.Util where

import Control.Effect.Throw

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
