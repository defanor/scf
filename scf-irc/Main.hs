{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import System.Environment
import Control.Applicative
import Data.HashMap.Strict
import Network.SimpleIRC
import Data.Time.Clock.POSIX
import Control.Concurrent
import Codec.Binary.UTF8.String

import SCF


data Msg = Msg { who :: String
               , message :: String
               , thread :: Maybe String
               } deriving (Show)

instance FromJSON Msg where
    parseJSON (Object v) = Msg
                           <$> v .: "to"
                           <*> v .: "message"
                           <*> v .:? "thread"
    parseJSON _ = mzero

instance ToJSON Msg where
  toJSON (Msg f m th) = object ["from" .= f, "message" .= m, "thread" .= th]

writer :: MIrc -> IO (Either String ())
writer irc = withJson $ \(Msg t m th) -> do
      mapM_ (\line -> sendMsg
                      irc
                      (BS.pack $ utf8Encode t)
                      (BS.pack $ utf8Encode line))
        (lines m)
      pure $ Right ()

onMsg :: IrcEvent
onMsg = Privmsg $ \irc m -> case mNick m of
  Just n -> do
    myNick <- getNickname irc
    let thread = if mChan m == Just myNick
                 then Nothing
                 else BS.unpack <$> mChan m
    prettyJson $  Msg (BS.unpack n) (decodeString $ BS.unpack $ mMsg m) thread
  Nothing -> putStrLn $ show m

onDisconnect :: IrcEvent
onDisconnect = Disconnect $ \irc -> reconnect' irc
  where
    reconnect' irc = do
      r <- reconnect irc
      case r of
        Left err -> do
          threadDelay $ 30 * 10 ^ 6
          reconnect' irc
        Right irc -> pure ()

onNumeric :: IrcEvent
onNumeric = Numeric $ \irc m -> case (mCode m) of
  "433" -> do
    nick <- getNickname irc
    t <- getPOSIXTime
    sendCmd irc . MNick $ BS.append nick . BS.pack . take 8 . drop 6 . show $ fromEnum t
  _ -> return ()


main :: IO ()
main = do
  args <- getArgs
  case args of
    (host:port:tls:nick:channels) -> do
      irc' <- connect (IrcConfig host (read port) (read tls)
                       nick Nothing nick nick channels
                       [onMsg, onDisconnect, onNumeric] "scf-irc" (pure "time") (3*10^8)) True False
      case irc' of
        Left err -> putStrLn $ show err
        Right irc -> do
          writer irc
          disconnect irc "bye"
          pure ()
    _ -> putStrLn "args: host, port, tls, nick, channels"
