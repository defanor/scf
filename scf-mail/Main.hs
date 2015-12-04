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
import Text.Parsec
import Control.Applicative hiding (many)
import Data.Maybe

import qualified Control.Exception as E
import Control.Monad
import Control.Concurrent

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as BS

import System.IO
import System.Environment

import Codec.MIME.Parse as MP
import Codec.MIME.Type as MT
import qualified Data.Text as T
import Codec.Text.IConv
import qualified Codec.MIME.QuotedPrintable as QP
import Codec.Binary.UTF8.String

import SCF


data Letter = Letter { lMessage :: String
                     , lTo :: String
                     , lFrom :: Maybe String
                     , lSubject :: Maybe String
                     , lID :: Maybe String
                     , lInReplyTo :: Maybe String
                     , lML :: Maybe String
                     , lDate :: Maybe String
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
                         <*> v .:? "date"
  parseJSON _ = mzero


instance ToJSON Letter where
  toJSON (Letter msg to from subj id irt ml date) =
    object ["to" .= to,
            "from" .= from,
            "subject" .= subj,
            "message" .= msg,
            "id" .= id,
            "in-reply-to" .= irt,
            "thread" .= ml,
            "date" .= date]

parseLetter :: BS.ByteString -> Maybe Letter
parseLetter s = letter <$> msg <*> h "to"
  where
    letter msg to = Letter msg (strip to) (strip <$> h "from") (h "subject")
                    (strip <$> h "message-id") (strip <$> h "in-reply-to")
                    (h "mailing-list") (h "date")
    msg = decodeString . filter (/= '\r')
          <$> case (mime_val_type mime, mime_val_content mime) of
      (MT.Type _ [MIMEParam "charset" c], Single s) ->
        case (c, convertStrictly (T.unpack $ T.toUpper c) "UTF-8" (t2bl s)) of
          ("utf-8", _) -> Just $ T.unpack s
          (_, Left bl) -> Just $ BL.unpack bl
          _ -> Just $ (T.unpack $ T.toUpper c)
      _ -> Nothing
    t2bl = BL.pack . T.unpack
    bs2t = T.pack . BS.unpack
    mime = parseMIMEMessage $ bs2t s
    h s = T.unpack . paramValue
          <$> (listToMaybe $ filter (\x -> paramName x == s) (mime_val_headers mime))
    strip s = case dropWhile (/= '<') s of
          [] -> s
          st -> takeWhile (/= '>') $ tail st

fetchNew :: IMAPConnection -> MailboxName -> IO [Letter]
fetchNew c mb = do
  select c mb
  new <- search c [NEWs]
  raw <- mapM (fetch c) new
  -- todo: log error messages here
  pure $ catMaybes $ map parseLetter raw

readLetters :: String -> UserName -> Password -> MailboxName -> IO ()
readLetters host user pass mailbox = forever . tryse $ do
    c <- connectIMAPSSLWithSettings host defaultSettingsIMAPSSL
    IMAP.authenticate c LOGIN user pass
    tryse . forever $ do
      letters <- fetchNew c mailbox
      mapM_ prettyJson letters
      hFlush stdout
      threadDelay $ 2 * 10 ^ 7
    logout c
    threadDelay $ 3 * 10 ^ 7
  where
    tryse :: IO () -> IO (Either E.SomeException ())
    tryse = E.try

sendLetter :: String -> UserName -> Password -> Letter -> IO ()
sendLetter host user pass l =
  doSMTPSSLWithSettings host defaultSettingsSMTPSSL $ \smtp -> do
    authSucceed <- SMTP.authenticate LOGIN user pass smtp
    when authSucceed $ do
      sendMail from [lTo l] msg smtp
  where
    from = user ++ "@" ++ host
    msg = BS.concat [headers, "\r\n\r\n", BS.pack (QP.encode . utf8Encode $ lMessage l)]
    headers = BS.intercalate "\r\n" .
              map (\(x,y) -> BS.concat [x, ": ", BS.pack y]) .
              filter ((/= "") . snd) $
              [("Date", fromMaybe "" (lDate l)),
               ("Message-Id", maybe "" (\x -> '<':x ++ ">") (lID l)),
               ("To", lTo l),
               ("Subject", fromMaybe "" (lSubject l)),
               ("From", fromMaybe from (lFrom l)),
               ("Sender", from),
               ("X-Mailer", "scf-mail"),
               ("Mime-Version", "1.0"),
               ("Content-Type", "text/plain; charset=utf-8"),
               ("Content-Transfer-Encoding", "quoted-printable"),
               ("In-Reply-To", maybe "" (\x -> '<':x ++ ">") (lInReplyTo l)),
               ("List-ID", fromMaybe "" (lML l))]

writer :: String -> UserName -> Password -> IO (Either String E.SomeException)
writer host user pass = withJson $ E.try . sendLetter host user pass

main :: IO ()
main = do
  args <- getArgs
  case args of
    [host, user, pass, mailbox] -> do
      forkIO $ readLetters host user pass mailbox
      r <- writer host user pass
      -- todo: check the result, possibly restart
      pure ()
    _ -> putStrLn "args: host, user, pass, mailbox"
