{-# LANGUAGE ExplicitNamespaces #-}

module Lib where

import           Api                         (SimpleList (..), SimpleListAPI,
                                              SimpleListEntry, simpleListApi)
import           Configuration.Dotenv        (defaultConfig, loadFile,
                                              onMissingFile)
import           Control.Exception           (catch)
import           Control.Monad.IO.Class      (MonadIO (liftIO))
import           Control.Monad.Trans.Reader  (ReaderT (runReaderT))
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8       as BS
import           Data.Int
import qualified Data.Text                   as T
import           Data.Text.Encoding          (encodeUtf8)
import           Db                          (SimpleListDb (entries), deleteSLE,
                                              getAll, insertSLE, updateSLE)
import           Network.Wai                 (Middleware)
import           Network.Wai.Handler.Warp    (run)
import           Network.Wai.Middleware.Cors
import           Servant                     (Application, Handler,
                                              HasServer (ServerT),
                                              NoContent (NoContent), Server,
                                              hoistServer, serve,
                                              serveDirectoryFileServer,
                                              type (:<|>) (..))
import           Shared
import           System.Directory            (getCurrentDirectory,
                                              listDirectory)
import           System.Environment          (getEnvironment)
import           System.Environment.MrEnv    (envAsInt, envAsString)

-- Middlewares
corsMiddleware :: [Origin] -> Middleware
corsMiddleware origins = cors (const $ Just policy)
  where
    policy =
      simpleCorsResourcePolicy
        { corsMethods = ["GET", "POST", "PUT", "DELETE"],
          corsRequestHeaders = simpleHeaders,
          corsOrigins = Just (origins, True)
        }

handlerSimpleList :: CustomAppM SimpleList
handlerSimpleList = do
  entries <- getAll
  return SimpleList {entries = entries}

-- curl -X POST -H "Content-Type: application/json" -d '{"id":11,"isDone":false,"text":"Buy groceries"}' http://localhost:9292/api/v1/entries -v
handlerPostSimpleList :: SimpleListEntry -> CustomAppM NoContent
handlerPostSimpleList e = do
  insertSLE e
  return NoContent

handlerPutSimpleList :: SimpleListEntry -> CustomAppM NoContent
handlerPutSimpleList e = do
  updateSLE e
  return NoContent

handlerDeleteSimpleList :: Int32 -> CustomAppM NoContent
handlerDeleteSimpleList id = do
  deleteSLE id
  return NoContent

server :: ServerT SimpleListAPI CustomAppM
server = (handlerSimpleList :<|> handlerPostSimpleList :<|> handlerPutSimpleList :<|> handlerDeleteSimpleList) :<|> serveDirectoryFileServer "static/"

nt :: AppState -> CustomAppM a -> Handler a
nt s x = runReaderT x s

app :: AppState -> Application
app s = serve simpleListApi $ hoistServer simpleListApi (nt s) server

createDb :: IO ()
createDb = undefined

splitOrigins :: String -> [ByteString]
splitOrigins s =
  let ts = T.pack s
      result = T.splitOn "," ts
   in encodeUtf8 <$> result

main :: IO ()
main = do
  -- Read argument and create DB
  cwd <- getCurrentDirectory
  putStrLn $ "Current working directory: " <> show cwd

  contents <- listDirectory cwd
  putStrLn "Files in the current directory:"
  mapM_ putStrLn contents

  -- Load .env variables
  onMissingFile (loadFile defaultConfig) (putStrLn "Warning: .env is missing.")

  env <- getEnvironment
  putStrLn "Environment Variables:"
  mapM_ (\(key, value) -> putStrLn $ key ++ "=" ++ value) env

  cs <- envAsString "DB_CONN_STRING" ""
  port <- envAsInt "PORT" 9292
  allowed_cors_origins <- envAsString "ALLOWED_CORS_ORIGINS_COMMA_SEPARATED" "http://localhost:9292"

  let initialState = AppState {db_conn_string = cs}

  putStrLn $ "Running at http://localhost:" <> show port <> "/api/v1/entries"
  run port (corsMiddleware (splitOrigins allowed_cors_origins) $ app initialState)
