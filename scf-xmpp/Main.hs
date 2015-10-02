{-# LANGUAGE OverloadedStrings #-}

import Network.Xmpp
import Network.Xmpp.Internal
import Control.Applicative
import Control.Monad
import Control.Concurrent
import Data.Text as T
import Control.Exception as E

import Data.Aeson
import Data.Maybe
import qualified Data.ByteString as BS

import System.Environment

import SCF


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
   -- filtering '\1', since some XMPP servers drop connection on it
   (MessageBody Nothing . T.filter (/= '\1') <$> maybeToList m))

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
reader s = forever $ getMessage s >>= prettyJson

writer :: Session -> IO (Either String XmppFailure)
writer s = withJson $ flip sendMessage s

main :: IO ()
main = do
  args <- getArgs
  case args of
    [host, name, pass] -> do
      let reconn s _ = do
            reconnect' s
            void $ sendPresence presenceOnline s
      sess' <- session host (simpleAuth (T.pack name) (T.pack pass)) $
               def {onConnectionClosed=reconn}
      case sess' of
        Left f -> putStrLn $ show f
        Right sess -> do
          sendPresence presenceOnline sess
          rt <- forkIO $ reader sess
          r <- writer sess
          -- something happened, clean up
          (E.try :: IO a -> IO (Either SomeException a)) $ do
            killThread rt
            sendPresence presenceOffline sess
            endSession sess
          case r of
            Right _ -> main -- an XmppFailure, restart
            _ -> pure () -- reading error, quit
    _ -> putStrLn "args: host, name, pass"
