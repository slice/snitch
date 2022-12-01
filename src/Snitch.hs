module Snitch (pollBy, poll) where

import Control.Monad (when)

-- | Compare adjacent, side-effected values with a comparator.
pollBy :: Monad m => a -> m a -> (a -> m ()) -> (a -> a -> Bool) -> m ()
pollBy initial gen act cmp = do
  next <- gen
  when (cmp initial next) (act next)
  pollBy next gen act cmp

-- | Compare adjacent, side-effected values with equality.
poll :: (Monad m, Eq a) => a -> m a -> (a -> m ()) -> m ()
poll initial gen act = pollBy initial gen act (/=)
