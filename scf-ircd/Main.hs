{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Pipes
import Pipes.Aeson as PA
import qualified Pipes.ByteString as PBS
import Control.Applicative hiding ((<|>))
import Control.Monad.State.Strict
import Network
import Control.Concurrent
import Control.Concurrent.Chan
import GHC.IO.Handle
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec
import qualified Data.ByteString.Char8 as BS
import Data.List
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson.Encode.Pretty
import Control.Exception as E
import Data.HashMap.Strict as HM hiding (map)
import System.IO
import System.Environment
import Numeric

data Msg = Msg { to :: Maybe String
               , from :: String
               , message :: String
               , thread :: Maybe String
               } deriving (Show)

instance FromJSON Msg where
    parseJSON (Object v) = Msg
                           <$> v .:? "to"
                           <*> v .: "from"
                           <*> v .: "message"
                           <*> v .:? "thread"
    parseJSON _ = mzero

instance ToJSON Msg where
  toJSON (Msg t f m th) = object ["to" .= t,
                                  "from" .= f,
                                  "message" .= m,
                                  "thread" .= th]


data Prefix = User String | Server String deriving (Show)

data IRCMsg = IRCMsg { prefix :: Maybe Prefix
                     , command :: String
                     , params :: [String]
                     } deriving (Show)

printIRCMsg :: IRCMsg -> String
printIRCMsg (IRCMsg pref c p) = prefix ++ c ++ " " ++ intercalate " " (map param p) ++ "\r\n"
  where
    param par = if elem ' ' par
                then ":" ++ par
                else par
    prefix = case pref of
      Nothing -> ""
      Just (User pr) -> ':' : pr ++ " "
      Just (Server pr) -> ':' : pr ++ " "

pMiddle :: Parser String
pMiddle = manyTill anyChar (choice [string " ", string "\r\n"] *> pure ())

pTrailing :: Parser String
pTrailing = (manyTill anyChar $ string "\r\n")
            <?> "trailing"

pParam :: Parser String
pParam = choice [char ':' *> pTrailing,
                 pMiddle]
         <?> "parameter"

pCommand :: Parser String
pCommand = pMiddle <?> "command"

pPrefix :: Parser Prefix
pPrefix = char ':'
           *> choice [User <$> ((some $ noneOf "\r\n !") <* (char '!' *> pMiddle)),
                      Server <$> pMiddle]
          <?> "prefix"

pMessage :: Parser IRCMsg
pMessage = IRCMsg
           <$> optionMaybe pPrefix
           <*> pCommand
           <*> Text.Parsec.many (Text.Parsec.try pParam)
           <?> "message"

readJson :: Chan (Either Msg IRCMsg) -> Producer PBS.ByteString IO x -> IO ()
readJson c p = do
  (r, p') <- runStateT PA.decode p
  case r of
    Nothing -> pure ()
    Just r' -> case r' of
      Left err -> readJson c p -- parse error, ignore garbage
      Right m -> do
        writeChan c (Left m)
        readJson c p'

readIRC :: Chan (Either Msg IRCMsg) -> Handle -> IO ()
readIRC c h = do
  r :: Either SomeException () <- E.try $ do
    line <- hGetLine h
    case parse pMessage "message" (line ++ "\n") of
      Left e -> putStrLn $ show e
      Right m -> writeChan c (Right m)
  case r of
    Left err -> pure ()
    Right () -> readIRC c h

data St = St { nick :: String
             , ident :: String
             , channels :: HashMap String [String]
             } deriving (Show)

-- reads messages from Chan, writes to both client and stdout, and
-- maintains state
processor :: Chan (Either Msg IRCMsg) -> HostName -> Handle -> St -> IO ()
processor c hn h s = do
  hFlush stdout
  m <- readChan c
  case m of
    Left (Msg t f m Nothing) -> do
      writeIRC $ IRCMsg (Just (User f)) "PRIVMSG" [nick s, m]
      processor c hn h s
    Left (Msg t f m (Just th)) -> do
      -- check if we're there already
      s' <- case HM.lookup th (channels s) of
        Nothing -> do
          writeIRC $ IRCMsg user "JOIN" [th]
          writeIRC $ IRCMsg server "353" [nick s, "@", th, nick s]
          writeIRC $ IRCMsg server "366" [nick s, th, "End of /NAMES list."]
          pure $ s {channels = HM.insert th [nick s] (channels s)}
        Just users -> pure s
      -- now check if the message author is there
      s'' <- case HM.lookup th (channels s') of
        Nothing -> pure s -- shouldn't happen
        Just users -> if f `elem` users
                      then pure s'
                      else do
                        writeIRC $ IRCMsg (Just $ User f) "JOIN" [th]
                        pure $ s {channels = HM.insert th (f:users) (channels s')}
      -- finally, write a message
      writeIRC $ IRCMsg (Just (User f)) "PRIVMSG" [th, m]
      processor c hn h $ s''
    Right (IRCMsg _ "PRIVMSG" [chan@('#':_), msg]) -> do
      BL.putStrLn . encodePretty $ Msg (Just chan) (nick s) msg (Just chan)
      processor c hn h s
    Right (IRCMsg _ "PRIVMSG" [name, msg]) -> do
      BL.putStrLn . encodePretty $ Msg (Just name) (nick s) msg Nothing
      processor c hn h s
    Right (IRCMsg _ "CAP" ["LS"]) -> do
      writeIRC $ IRCMsg server "CAP" ["*", "LS", "incapable of anything"]
      processor c hn h s
    Right (IRCMsg _ "NICK" [nick]) -> processor c hn h $ s { nick = nick }
    Right (IRCMsg _ "JOIN" [channel]) -> do
      writeIRC $ IRCMsg user "JOIN" [channel]
      writeIRC $ IRCMsg server "353" [nick s, "@", channel, nick s]
      writeIRC $ IRCMsg server "366" [nick s, channel, "End of /NAMES list."]
      processor c hn h $ s {channels = HM.insert channel [nick s] (channels s)}
    Right (IRCMsg _ "USER" [ident, _, _, name]) -> do
      writeIRC $ IRCMsg server "001" [nick s, "Welcome"]
      writeIRC $ IRCMsg server "002" [nick s, "Your host is " ++ hn ++ ", running scf-ircd"]
      writeIRC $ IRCMsg server "003" [nick s, "This server was created just now"]
      writeIRC $ IRCMsg server "004" [nick s, sname ++ " scf-ircd"]
      writeIRC $ IRCMsg server "005" [nick s, "CHANTYPES=# CHARSET=ascii PREFIX=(ov)@+ CHANMODES= MODES=4 NETWORK=scf MAXCHANNELS=42 NICKLEN=42 CASEMAPPING=ascii TOPICLEN=42 CHANNELLEN=42 KICKLEN=42"]
      writeIRC $ IRCMsg server "MODE" [nick s, "+i"]
      processor c hn h $ s { ident = ident }
    Right _ -> processor c hn h s
  where
    user :: Maybe Prefix
    user = Just $ User $ nick s ++ "!" ++ ident s ++ "@" ++ hn
    server :: Maybe Prefix
    server = Just $ Server sname
    sname :: String
    sname = "scf-ircd.local"
    writeIRC :: IRCMsg -> IO ()
    writeIRC = hPutStr h . printIRCMsg


ircd :: Int -> Chan (Either Msg IRCMsg) -> IO ()
ircd p c = do
  s <- listenOn (PortNumber $ fromIntegral p)
  (h, hn, pn) <- accept s
  hSetEncoding h utf8
  msgProcessor <- forkIO $ processor c hn h (St "" "" HM.empty)
  readIRC c h
  sClose s
  killThread msgProcessor
  ircd p c

main :: IO ()
main = do
  args <- getArgs
  case args of
    [port] -> case (readDec port) of
      [(p, "")] -> do
        c <- newChan
        irc <- forkIO $ ircd p c
        readJson c PBS.stdin
        killThread irc
      _ -> putStrLn "failed to parse port"
    _ -> putStrLn "args: port"
