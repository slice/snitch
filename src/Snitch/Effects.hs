-- | Algebraic effects.
module Snitch.Effects (Http (..), get, Response (..), HttpReqC (..)) where

import Control.Algebra
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Network.HTTP.Req qualified as Req

{- | An effect for making HTTP requests.

 Snitch makes HTTP requests to fetch data from the network. Because
 Snitch is not concerned with the exact way in which this is done (and it should
 not be), an effect is used so that the exact behavior can be determined by the
 consumer of the API.
-}
data Http (m :: Type -> Type) k where
  Get :: Req.Url 'Req.Https -> Http m Response

get :: Has Http sig m => Req.Url 'Req.Https -> m Response
get = send . Get

newtype HttpReqC m a = HttpReqC {runHttpReq :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance (MonadIO m, Algebra sig m) => Algebra (Http :+: sig) (HttpReqC m) where
  alg hdl sig ctx = case sig of
    L (Get url) -> liftIO $ Req.runReq Req.defaultHttpConfig do
      resp <- Req.req Req.GET url Req.NoReqBody Req.bsResponse mempty
      let body = Req.responseBody resp
      let response = Response body (Req.responseHeader resp)
      pure (response <$ ctx)
    R other -> HttpReqC (alg (runHttpReq . hdl) other ctx)

-- | An HTTP response returned by 'Http'.
data Response = Response
  { responseBody :: ByteString
  -- ^ The entire response body returned by the server.
  , responseLookupHeader :: ByteString -> Maybe ByteString
  -- ^ Looks up a header in the response.
  }
