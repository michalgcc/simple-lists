module SimpleLists.Repository.Todo
  ( createTodoEntry
  , deleteTodoList
  , getTodoList
  , updateTodoList
  ) where

import Prelude
import Affjax (printError)
import Affjax.RequestBody (RequestBody(..))
import Affjax.ResponseFormat (ignore, json)
import Affjax.Web (defaultRequest, request)
import SimpleLists.Config (baseUrl)
import Data.Argonaut (decodeJson, encodeJson)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import SimpleLists.Data.Todo (Todo, TodoEntry)
import Effect.Aff (Aff)

-- TODO take it from env variable
-- Create base url resolver from env
-- Based on build process defined in spago.dhall
getBaseUrl :: String
getBaseUrl = baseUrl

getTodoList :: Aff (Either String Todo)
getTodoList = do
  let
    todoRequest = defaultRequest { url = getBaseUrl <> "/api/v1/entries", method = Left GET, responseFormat = json }
  response <- request todoRequest
  case response of
    Left err -> pure $ Left (printError err)
    Right res -> do
      case decodeJson res.body of
        Right (r :: Todo) -> pure $ Right r
        Left e -> do
          pure $ Left ("Can't parse JSON. " <> show e)

updateTodoList :: TodoEntry -> Aff (Either String String)
updateTodoList tle = do
  let
    body = encodeJson tle

    updateRequest = defaultRequest { url = getBaseUrl <> "/api/v1/entries", method = Left PUT, responseFormat = ignore, content = Just $ Json body }
  response <- request updateRequest
  case response of
    Left err -> pure $ Left (printError err)
    Right res -> pure $ Right ("Change me to unit")

deleteTodoList :: Int -> Aff (Either String String)
deleteTodoList id = do
  let
    deleteRequest = defaultRequest { url = getBaseUrl <> "/api/v1/entries/" <> show id, method = Left DELETE, responseFormat = ignore }
  response <- request deleteRequest
  case response of
    Left err -> pure $ Left (printError err)
    Right res -> pure $ Right ("Change me to unit")

createTodoEntry :: TodoEntry -> Aff (Either String String)
createTodoEntry tle = do
  let
    body = encodeJson tle

    updateRequest = defaultRequest { url = getBaseUrl <> "/api/v1/entries", method = Left POST, responseFormat = ignore, content = Just $ Json body }
  response <- request updateRequest
  case response of
    Left err -> pure $ Left (printError err)
    Right res -> pure $ Right ("Change me to unit")
