{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Pipes
import Pipes.Aeson as PA
import qualified Pipes.ByteString as PBS
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Monad.State.Strict
import Data.HashMap.Strict as H hiding (map, filter)
import Data.Text as T
import qualified Data.List as L
import Data.Char
import System.IO

processMessage :: [Text] -> Object -> Text -> Text -> IO [Text]
processMessage u o f m
  | isPrefixOf "[join]" m = processMessage (f:u) o f (T.drop 6 m)
  | isPrefixOf "[part]" m = processMessage (L.delete f u) o f (T.drop 6 m)
  | T.all isSpace m = pure u
  | otherwise = do
      mapM_ (\x -> BL.putStrLn . encodePretty $ insert "to" (String x) o) u
      hFlush stdout
      pure u

readJson :: Text -> [Text] -> Producer PBS.ByteString IO x -> IO ()
readJson last u p = do
  (r, p') <- runStateT PA.decode p
  case r of
    Nothing -> pure ()
    Just r' -> case r' of
      Right (Object v) -> case (H.lookup "from" v, H.lookup "message" v) of
        (Just (String f), Just (String m)) -> processMessage u v f m >>= flip readJson p'
        _ -> readJson u p'
      _ -> pure ()

main :: IO ()
main = do
  readJson [] PBS.stdin
