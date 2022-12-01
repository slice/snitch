-- | Parsers for more elaborate data extraction.
module Snitch.Parser (appBuildNumberParser, buildInfoHeading) where

import Control.Monad (void)
import Data.Text (Text)
import Data.Void (Void)
import Snitch.Discord (BuildNumber (..))
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

quoted :: (MonadParsec e s m, Token s ~ Char) => m a -> m a
quoted = between (char '"') (char '"')

parenthesized :: (MonadParsec e s m, Token s ~ Char) => m a -> m a
parenthesized = between (char '(') (char ')')

literally :: Text -> Parsec Void Text ()
literally = void . string

{- | A piece of text present inside of the entrypoint script which is adjacent
 to build information we're interested in.

 This value is literally:

 > [BUILD INFO] Release Channel:

 with a single space at the end.
-}
buildInfoHeading :: Text
buildInfoHeading = "[BUILD INFO] Release Channel: "

{- | Parses a fragment of JavaScript indicating the build number.

 An example of the fragment parsed:

 > "[BUILD INFO] Release Channel: ".concat(Ee,", Build Number: ").concat("160745"

 This parses to a 'BuildNumber' of 160745.
-}
appBuildNumberParser :: Parsec Void Text BuildNumber
appBuildNumberParser = do
  -- TODO: Support the old style in case Discord decides to stop using swc.
  _ <- quoted . string $ buildInfoHeading
  literally ".concat"
  _ <- parenthesized $ some letterChar *> char ',' *> quoted ", Build Number: "
  literally ".concat("
  BuildNumber <$> quoted decimal
