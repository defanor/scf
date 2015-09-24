{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP
import Text.Feed.Import
import Text.Feed.Types
import Text.Atom.Feed
import Data.Aeson
import Data.Default
import Control.Applicative
import Data.List
import Control.Concurrent
import qualified Data.ByteString.Lazy.Char8 as BL
import System.Environment
import System.IO

import SCF

data Msg = Msg { mFrom :: Maybe String
               , mThread :: String
               , mID :: String
               , mSubject :: String
               , mMessage :: String
               } deriving (Show, Eq)

instance Default Msg where
  def = Msg (Just "default") "default" "default" "default" "default"

instance ToJSON Msg where
  toJSON (Msg f t i s m) = object ["from" .=  f,
                                   "thread" .= t,
                                   "id" .= i,
                                   "subject" .= s,
                                   "message" .= m]

atom :: String -> Entry -> Msg
atom t e = let
  links = intercalate ", " . map linkHref $ entryLinks e
  message = case entrySummary e of
    Just (TextString s) -> s ++ " | " ++ links
    _ -> links
  from = case entryAuthors e of
    x:xs -> personEmail x <|> personURI x <|> pure (personName x)
    _ -> Nothing
  in Msg from t (entryId e) (txtToString $ entryTitle e) message

getFeed :: String -> IO [Msg]
getFeed u = do
  r <- simpleHTTP (getRequest u)
  b <- getResponseBody r
  return $ case parseFeedString b of
    Nothing -> []
    Just (AtomFeed f) -> map (atom $ feedId f) $ feedEntries f
    Just _ -> []

feed :: Msg -> String -> IO ()
feed m u = do
  a <- takeWhile (/= m) <$> getFeed u
  mapM_ prettyJson a
  hFlush stdout
  threadDelay $ 9 * 10 ^ 8 -- 15 minutes
  case a of
    [] -> feed m u
    (l:_) -> feed l u

-- todo: make it concurrent
runFeed :: String -> IO ()
runFeed u = do
  a <- getFeed u
  case a of
    [] -> feed def u
    l:_ -> feed def u

main :: IO ()
main = getArgs >>= mapM_ runFeed >> getLine >> pure ()
