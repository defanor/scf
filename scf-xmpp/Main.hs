{-# LANGUAGE OverloadedStrings #-}

import Network.Xmpp
import Network.Xmpp.Internal
import Data.Default
import Network.TLS
import Data.X509.Validation
import Control.Applicative
import Control.Monad
import Data.Monoid
import Control.Concurrent
import qualified Data.Text as T

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import           Network.TLS hiding (Version)
import           Network.TLS.Extra
import           Network

import Numeric

import Data.Aeson as A
import Data.Maybe
import Data.XML.Types

import Pipes
import Pipes.Aeson
import qualified Pipes.ByteString as PBS

import Control.Monad.State.Strict
import Pipes.Attoparsec

import System.Environment

import Data.Aeson.Encode.Pretty

import System.IO


instance FromJSON Jid where
  parseJSON (String s) = case jidFromText s of
    Just r -> pure r
    _ -> mzero
  parseJSON _ = mzero

instance ToJSON Jid where
  toJSON (Jid name host res) =
    String $ T.concat [maybe "" ((flip T.append "@") . fromNonempty) name,
                       fromNonempty host,
                       maybe "" ((T.append "/") . fromNonempty) res]

data Msg = Msg { mMessage :: Maybe T.Text
               , mTo :: Maybe Jid
               , mFrom :: Maybe Jid
               , mID :: Maybe T.Text
               , mThread :: Maybe T.Text
               , mSubject :: Maybe T.Text
               } deriving (Show)

instance FromJSON Msg where
    parseJSON (Object v) = Msg
                           <$> v .:? "message"
                           <*> v .:? "to"
                           <*> v .:? "from"
                           <*> v .:? "id"
                           <*> v .:? "thread"
                           <*> v .:? "subject"
    parseJSON _ = mzero

instance ToJSON Msg where
  toJSON (Msg m t f i th s) = object ["id" .= i,
                                      "from" .= f,
                                      "to" .= t,
                                      "subject" .= s,
                                      "thread" .= th,
                                      "message" .= m]

encodeMsg :: Msg -> Message
encodeMsg (Msg m t f i th s) =
  withIM (message {messageTo = t,
                   messageFrom = f,
                   messageID = i})
  (InstantMessage
   (flip MessageThread Nothing <$> th)
   (MessageSubject Nothing <$> maybeToList s)
   (MessageBody Nothing <$> maybeToList m))

decodeMsg :: Message -> Msg
decodeMsg m@(Message i f t _ _ _ _) = case getIM m of
  Nothing -> Msg Nothing t f i Nothing Nothing
  Just (InstantMessage th s b) ->
    Msg (bodyContent <$> listToMaybe b) t f i (threadID <$> th) (subjectContent <$> listToMaybe s)

instance FromJSON Message where
  parseJSON = fmap encodeMsg . parseJSON

instance ToJSON Message where
  toJSON = toJSON . decodeMsg

reader :: Session -> IO ()
reader s = forever $ do
  msg <- getMessage s
  BL.putStrLn $ encodePretty msg
  hFlush stdout

writer :: Session -> Producer PBS.ByteString IO x -> IO ()
writer s p = do
  (r, p') <- runStateT (Pipes.Aeson.decode :: PBS.Parser PBS.ByteString IO (Maybe (Either DecodingError Message))) p
  case r of
    Nothing -> pure ()
    Just r' -> do
      case r' of
        Left err -> putStrLn (show err)
        Right msg -> do
          sent <- sendMessage msg s
          case sent of
            Right () -> writer s p'
            Left err -> putStrLn $ show err


main :: IO ()
main = do
  args <- getArgs
  case args of
    [host, name, pass] -> do
      sess' <- session host (simpleAuth (T.pack name) (T.pack pass)) def
      case sess' of
        Left f -> putStrLn $ show f
        Right sess -> do
          _ <- sendPresence presenceOnline sess
          forkIO $ reader sess
          writer sess PBS.stdin
    _ -> putStrLn "args: host, name, pass"

