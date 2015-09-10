{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson hiding (decode)
import Pipes
import Pipes.Aeson
import Pipes.Safe
import Pipes.Safe.Prelude as PSP
import qualified Pipes.ByteString as PBS
import Control.Monad.State.Strict
import Pipes.Attoparsec
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as BS

import System.Environment

import Data.Aeson.Encode.Pretty

import Control.Applicative
import Data.HashMap.Strict as H hiding (map)

import qualified Data.Text as T

import Data.Attoparsec.Text as A
import Data.Char

import System.IO

import Control.Lens (view)
import Pipes.Aeson.Unchecked as AU (decoded)
import Pipes.Concurrent
import Control.Concurrent (ThreadId)

import qualified GHC.IO.Exception as G
import Foreign.C.Error (Errno(Errno), ePIPE)
import Control.Exception as E (throwIO, try)

keyFilter :: (Value -> Bool) -> Object -> T.Text -> Maybe Object
keyFilter f o k = H.lookup k o >>=
                  \x -> if f x
                        then pure o
                        else mempty

fuseVal :: T.Text -> T.Text -> T.Text -> Object -> Object
fuseVal k before after o = case (H.lookup k o, H.lookup "message" o) of
  (Just (String v), Just (String msg)) ->
    insert "message" (String $ T.concat [before, v, after, msg]) $
    delete k o
  _ -> o

extractVal :: T.Text -> T.Text -> T.Text -> Object -> Object
extractVal k before after o = case H.lookup "message" o of
  Just (String msg) -> case A.parse parser msg of
    Done rest v -> insert k (String $ T.pack v) $ insert "message" (String rest) o
    _ -> o
  Nothing -> o
  where
    parser = string before *> manyTill (satisfy $ not . isSpace) (string after)

translate :: [T.Text] -> Object -> Object
translate ["swap"] o = let to = lookupDefault Null "from" o
                           from = lookupDefault Null "to" o
                       in insert "to" to $ insert "from" from o
translate ["set", k, v] o = insert k (String v) o
translate ["add", k, v] o = case lookupDefault (String v) k o of
  Null -> insert k (String v) o
  v' -> insert k v' o
translate ["setk", k, k2] o = insert k (lookupDefault Null k2 o) o
translate ["del", k] o = delete k o
translate ["fuse", k, b, a] o = fuseVal k b a o
translate ["fuse", k, a] o = fuseVal k "" a o
translate ["extract", k, b, a] o = extractVal k b a o
translate ["extract", k, a] o = extractVal k "" a o
translate s o = o

filter' :: [T.Text] -> Object -> Maybe Object
filter' ["eq", k, v] o = keyFilter (== (String v)) o k
filter' ["neq", k, v] o = keyFilter (/= (String v)) o k
filter' ["with", k] o = if member k o then pure o else mempty
filter' ["without", k] o = if member k o then mempty else pure o
filter' _ o = pure o




inp :: MonadIO m => Producer PBS.ByteString m r ->
       (Producer Object m (Either (DecodingError, Producer PBS.ByteString m r) r))
inp = view AU.decoded

readFile' :: MonadSafe m => FilePath -> Producer' BS.ByteString m ()
readFile' fp = for (PSP.readFile fp) (yield . BS.pack)

fromFile :: (MonadIO m, MonadSafe m) => FilePath ->
            Producer Object m (Either (DecodingError, Producer BS.ByteString m ()) ())
fromFile = inp . readFile'

runConsumer :: Output Object -> String -> IO ThreadId
runConsumer o p = forkIO $ do
  runSafeT $ runEffect $ (fromFile p >> pure ()) >-> toOutput o
  performGC

prettyOut :: MonadIO m => Consumer' Object m ()
prettyOut = go
  where
    go = do
        bs <- await
        x  <- liftIO $ E.try $ do
          BL.putStrLn $ encodePretty bs
          hFlush stdout
        case x of
            Left (G.IOError { G.ioe_type  = G.ResourceVanished
                            , G.ioe_errno = Just ioe })
                 | Errno ioe == ePIPE
                     -> return ()
            Left  e  -> liftIO (throwIO e)
            Right () -> go

merge :: [String] -> IO ()
merge pipes = do
  (o, i) <- spawn Unbounded
  forkIO $ do
    runSafeT $ runEffect $ (inp PBS.stdin >> pure ()) >-> toOutput o
    performGC
  mapM_ (runConsumer o) pipes
  runEffect $ fromInput i >-> prettyOut

process :: Monad m => [T.Text] -> Pipe Object Object m ()
process args = do
  obj <- await
  case filter' args $ translate args obj of
    Nothing -> pure ()
    Just v -> yield v
  process args


main :: IO ()
main = do
  args' <- getArgs
  case args' of
    "merge":xs -> merge xs
    _ -> do
      let args = map T.pack args'
      runEffect $ (inp PBS.stdin >> pure ()) >-> process args >-> prettyOut
