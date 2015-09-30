{-# LANGUAGE OverloadedStrings #-}
-- | Common JSON-related utilities.

module SCF.JSON (
  -- * Input
  withJson',
  withJson,
  readJson',
  readJson,
  -- * Output
  prettyJson
  ) where
import Data.Aeson
import System.IO
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Monad.State.Strict
import Data.Attoparsec.ByteString as AP
import Data.ByteString.Char8 as BS
import Control.Applicative
import Control.Exception as E


-- | Pretty-prints a value as JSON, and flushes 'stdout'.
prettyJson :: (MonadIO m, ToJSON a)
              => a
              -- ^ A value to print.
              -> m ()
prettyJson x = liftIO $ do
  BL.putStrLn $ encodePretty x
  hFlush stdout

withJson :: (Functor m, MonadIO m, FromJSON a)
            => (a -> m (Either e u))
            -> m (Either String e)
withJson f = withJson' $ fmap Right . f

withJson' :: (MonadIO m, FromJSON a)
            => (a -> m (Either e u))
            -> m (Either String e)
withJson' f = wJson ""
  where
    wJson s = do
      v <- readJson' s
      case v of
        Left err -> return $ Left err
        Right (j, r) -> do
          fr <- f r
          case fr of
            Left err -> return $ Right err
            Right _ -> wJson j

readJson :: (MonadIO m, FromJSON a)
            => m (Either String a)
readJson = readJson' "" >>= return . fmap snd

readJson' :: (MonadIO m, FromJSON a)
             => BS.ByteString
             -> m (Either String (BS.ByteString, a))
readJson' "" = withLine $ parse json'
readJson' s = continue $ parse json' s

continue :: (MonadIO m, FromJSON a) => AP.Result Value -> m (Either String (BS.ByteString, a))
continue (Fail i c err) = readJson' "" -- todo: log an error
continue p@(Partial f) = withLine $ feed p
continue (Done i r) = case fromJSON r of
  Error err -> readJson' "" -- todo: log an error
  Success r -> return $ Right (i, r)

withLine :: (MonadIO m, FromJSON a)
            => (BS.ByteString -> AP.Result Value)
            -> m (Either String (BS.ByteString, a))
withLine f = do
  l <- liftIO (E.try BS.getLine :: IO (Either SomeException BS.ByteString))
  case l of
    Left err -> return $ Left $ show err
    Right r -> continue . f $ BS.append r "\n"

