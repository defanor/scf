{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson
import Pipes
import Pipes.Aeson
import qualified Pipes.ByteString as PBS
import Control.Monad.State.Strict
import Pipes.Attoparsec
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as BS

import System.Environment

import Data.Aeson.Encode.Pretty

import Control.Applicative
import Data.HashMap.Strict as H hiding (map, filter)

import qualified Data.Text as T

import Data.Attoparsec.Text as A
import Data.Char

import Network.SimpleIRC

import System.IO



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

writer :: MIrc -> Producer PBS.ByteString IO x -> IO ()
writer irc p = do
  (r, p') <- runStateT (Pipes.Aeson.decode :: PBS.Parser PBS.ByteString IO (Maybe (Either DecodingError Msg))) p
  case r of
    Nothing -> pure ()
    Just r' -> do
      case r' of
        Left err -> putStrLn (show err)
        Right (Msg t m th) -> do
          sendMsg irc (BS.pack t) (BS.pack m)
          writer irc p'

onMsg :: IrcEvent
onMsg = Privmsg $ \irc m -> case mNick m of
  Just n -> do
    myNick <- getNickname irc
    let thread = if mChan m == Just myNick
                 then Nothing
                 else BS.unpack <$> mChan m
    BL.putStrLn . encodePretty $
      Msg (BS.unpack n) (filter (not . isControl) . BS.unpack $ mMsg m) thread
    hFlush stdout
  Nothing -> putStrLn $ show m


main :: IO ()
main = do
  args <- getArgs
  case args of
    (host:port:tls:nick:channels) -> do
      irc' <- connect (IrcConfig host (read port) (read tls)
                       nick Nothing nick nick channels
                       [onMsg] "scf-irc" (pure "time") 240000000) True False
      case irc' of
        Left err -> putStrLn $ show err
        Right irc -> writer irc PBS.stdin
    _ -> putStrLn "args: host, port, tls, nick, channels"

