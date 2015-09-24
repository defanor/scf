-- | Common JSON-related utilities.

module SCF.JSON (
  -- * Input
  QuitReason(..),
  withJson',
  withJson,
  -- * Output
  prettyJson
  ) where
import Data.Aeson
import Pipes
import Pipes.Aeson as PA
import qualified Pipes.ByteString as PBS
import System.IO
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Monad.State.Strict


-- | A reason why 'withJson' stops.
data QuitReason a = DE DecodingError
                    -- ^ Decoding error.
                  | EOF
                    -- ^ Input is exhausted.
                  | UM a
                    -- ^ User message.
                  deriving (Show)

-- | Repeatedly runs a given action with input from a given producer,
-- until either it returns 'Just', or some other exit condition
-- occurs.
withJson' :: (MonadIO m, FromJSON a, ToJSON a)
             => Producer PBS.ByteString m r
             -- ^ Input source.
             -> (a -> m (Maybe u))
             -- ^ An action to perform repeatedly.
             -> m (QuitReason u)
             -- ^ A reason why it stopped.
withJson' p f = do
  (r, p') <- runStateT PA.decode p
  case r of
    Nothing -> return EOF
    Just r' -> case r' of
      Left err -> return $ DE err
      Right x -> do
        b <- f x
        case b of
          Nothing -> withJson' p' f
          Just m -> return $ UM m

-- | A simplified version of 'withJson'': reads 'PBS.stdin' and the
-- provided action can not stop the process.
--
-- Most of the programs should use this function, quit when 'EOF' or
-- 'DE' occurs, and possibly restart after 'UM'.
withJson :: (MonadIO m, FromJSON a, ToJSON a)
            => (a -> m u)
            -- ^ An action to perform repeatedly.
            -> m (QuitReason u)
            -- ^ A reason why it stopped.
withJson f = withJson' PBS.stdin $ \x -> f x >> return Nothing

-- | Pretty-prints a value as JSON, and flushes 'stdout'.
prettyJson :: (MonadIO m, ToJSON a)
              => a
              -- ^ A value to print.
              -> m ()
prettyJson x = liftIO $ do
  BL.putStrLn $ encodePretty x
  hFlush stdout
