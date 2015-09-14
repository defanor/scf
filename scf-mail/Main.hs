{-# LANGUAGE OverloadedStrings #-}
import Network.HaskellNet.IMAP as IMAP
import Network.HaskellNet.IMAP.Types
import Network.HaskellNet.IMAP.Connection
import Network.HaskellNet.Auth hiding (login)
import Network.HaskellNet.IMAP.SSL
import Network.HaskellNet.SMTP as SMTP
import Network.HaskellNet.SMTP.SSL
import Network.Socket
import qualified Data.ByteString.Char8 as BS

import Text.Parsec.ByteString
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec
import Control.Applicative hiding (many)
import Data.Either
import Data.Maybe

import qualified Control.Exception as E
import Control.Monad
import Control.Concurrent

import Data.Aeson
import Data.Aeson.Encode.Pretty
import Pipes
import Pipes.Aeson as PA
import qualified Pipes.ByteString as PBS
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Monad.State.Strict

import System.IO
import System.Environment
import Numeric


data Letter = Letter { lMessage :: String
                     , lTo :: String
                     , lFrom :: Maybe String
                     , lSubject :: Maybe String
                     , lID :: Maybe String
                     , lInReplyTo :: Maybe String
                     , lML :: Maybe String
                     } deriving (Show)

instance FromJSON Letter where
  parseJSON (Object v) = Letter
                         <$> v .: "message"
                         <*> v .: "to"
                         <*> v .:? "from"
                         <*> v .:? "subject"
                         <*> v .:? "id"
                         <*> v .:? "in-reply-to"
                         <*> v .:? "thread"
  parseJSON _ = mzero


instance ToJSON Letter where
  toJSON (Letter msg to from subj id irt ml) =
    object ["to" .= to,
            "from" .= from,
            "subject" .= subj,
            "message" .= msg,
            "id" .= id,
            "in-reply-to" .= irt,
            "thread" .= ml]

mkLetter :: ([(String, String)], String) -> Maybe Letter
mkLetter (h, m) = compose <$> lookup "To" h
  where
    l = flip lookup h
    c :: String -> String
    c s = case dropWhile (/= '<') s of
      [] -> s
      st -> takeWhile (/= '>') $ tail st
    compose to = Letter m (c to) (c <$> l "From") (l "Subject")
                 (c <$> l "Message-Id") (c <$> l "In-Reply-To") (l "Mailing-list")

pHeader :: Parser (String, String)
pHeader = (,)
          <$> (manyTill (noneOf ": ") (string ": "))
          <*> manyTill anyChar (try (string "\r\n" >> lookAhead (noneOf "\t")))

pMessage :: Parser ([(String, String)], String)
pMessage = (,)
           <$> ((many (try pHeader)) <* string "\r\n")
           <*> many anyChar

fetchNew :: IMAPConnection -> MailboxName -> IO [Letter]
fetchNew c mb = do
  select c mb
  new <- search c [NEWs]
  raw <- mapM (fetch c) new
  let parsed = map (parse pMessage "letter") raw
  -- todo: log error messages here
  pure . catMaybes . map mkLetter $ rights parsed


readLetters :: String -> UserName -> Password -> MailboxName -> IO ()
readLetters host user pass mailbox = forever . tryse $ do
    c <- connectIMAPSSLWithSettings host defaultSettingsIMAPSSL {sslDisableCertificateValidation=True}
    IMAP.authenticate c LOGIN user pass
    tryse . forever $ do
      letters <- fetchNew c mailbox
      mapM_ (BL.putStrLn . encodePretty) letters
      hFlush stdout
      threadDelay $ 2 * 10 ^ 7
    logout c
    threadDelay $ 3 * 10 ^ 7
  where
    tryse :: IO () -> IO (Either E.SomeException ())
    tryse = E.try

sendLetter :: String -> UserName -> Password -> Letter -> IO ()
sendLetter host user pass l =
  doSMTPSSLWithSettings host
  (defaultSettingsSMTPSSL {sslDisableCertificateValidation=True}) $ \smtp -> do
    authSucceed <- SMTP.authenticate LOGIN user pass smtp
    when authSucceed $ do
      sendMail from [lTo l] msg smtp
  where
    from = user ++ "@" ++ host
    msg = BS.concat [headers, "\r\n\r\n", BS.pack (lMessage l)]
    headers = BS.intercalate "\r\n" .
              map (\(x,y) -> BS.concat [x, ": ", BS.pack y]) .
              filter ((/= "") . snd) $
              [("To", lTo l),
               ("From", maybe from id (lFrom l)),
               ("Subject", maybe "no subject" id (lSubject l)),
               ("Message-Id", maybe "" id (lID l)),
               ("In-Reply-To", maybe "" id (lInReplyTo l)),
               ("Mailing-list", maybe "" id (lML l))]

readJson :: String -> UserName -> Password -> Producer PBS.ByteString IO x -> IO ()
readJson host user pass p = do
  (r, p') <- runStateT PA.decode p
  case r of
    Nothing -> pure ()
    Just r' -> case r' of
      Left err -> putStrLn (show err)
      Right l -> do
        E.try (sendLetter host user pass l) :: IO (Either E.SomeException ())
        readJson host user pass p'


main :: IO ()
main = do
  args <- getArgs
  case args of
    [host, user, pass, mailbox] -> do
      forkIO $ readLetters host user pass mailbox
      readJson host user pass PBS.stdin
    _ -> putStrLn "args: host, user, pass, mailbox"
