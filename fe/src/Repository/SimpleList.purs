module SimpleLists.Repository.SimpleList
  ( createSimpleList
  , createSimpleListEntry
  , deleteSimpleList
  , deleteSimpleListEntry
  , getSimpleListEntries
  , getSimpleListEntriesByListId
  , getSimpleLists
  , updateSimpleList
  , updateSimpleListEntry
  ) where

import Prelude

import Affjax (printError)
import Affjax.RequestBody (RequestBody(..))
import Affjax.ResponseFormat (ignore, json)
import Affjax.Web (defaultRequest, request)
import Data.Argonaut (decodeJson, encodeJson)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import SimpleLists.Config (baseUrl)
import SimpleLists.Data.SimpleList (CreateSimpleList, CreateSimpleListEntry, SimpleList, SimpleListEntry, UpdateSimpleListEntry, UpdateSimpleList)

-- TODO take it from env variable
-- Create base url resolver from env
-- Based on build process defined in spago.dhall
getBaseUrl :: String
getBaseUrl = baseUrl

getSimpleLists :: Aff (Either String (Array SimpleList))
getSimpleLists = do
  let
    simpleListRequest = defaultRequest { url = getBaseUrl <> "/api/v1/lists", method = Left GET, responseFormat = json }
  response <- request simpleListRequest
  case response of
    Left err -> pure $ Left (printError err)
    Right res -> do
      case decodeJson res.body of
        Right (r :: Array SimpleList) -> pure $ Right r
        Left e -> do
          pure $ Left ("Can't parse JSON. " <> show e)

updateSimpleList :: UpdateSimpleList -> Aff (Either String String)
updateSimpleList tle = do
  let
    body = encodeJson tle

    updateRequest = defaultRequest { url = getBaseUrl <> "/api/v1/lists", method = Left PUT, responseFormat = ignore, content = Just $ Json body }
  response <- request updateRequest
  case response of
    Left err -> pure $ Left (printError err)
    Right res -> pure $ Right ("Change me to unit")

deleteSimpleList :: String -> Aff (Either String String)
deleteSimpleList id = do
  let
    deleteRequest = defaultRequest { url = getBaseUrl <> "/api/v1/lists/" <> id, method = Left DELETE, responseFormat = ignore }
  response <- request deleteRequest
  case response of
    Left err -> pure $ Left (printError err)
    Right res -> pure $ Right ("Change me to unit")

createSimpleList :: CreateSimpleList -> Aff (Either String String)
createSimpleList tle = do
  let
    body = encodeJson tle

    updateRequest = defaultRequest { url = getBaseUrl <> "/api/v1/lists", method = Left POST, responseFormat = ignore, content = Just $ Json body }
  response <- request updateRequest
  case response of
    Left err -> pure $ Left (printError err)
    Right res -> pure $ Right ("Change me to unit")

getSimpleListEntries :: Aff (Either String (Array SimpleListEntry))
getSimpleListEntries = do
  let
    simpleListRequest = defaultRequest { url = getBaseUrl <> "/api/v1/entries", method = Left GET, responseFormat = json }
  response <- request simpleListRequest
  case response of
    Left err -> pure $ Left (printError err)
    Right res -> do
      case decodeJson res.body of
        Right (r :: Array SimpleListEntry) -> pure $ Right r
        Left e -> do
          pure $ Left ("Can't parse JSON. " <> show e)

getSimpleListEntriesByListId :: String -> Aff (Either String (Array SimpleListEntry))
getSimpleListEntriesByListId id = do
  let
    simpleListRequest = defaultRequest { url = getBaseUrl <> "/api/v1/entries?listId=" <> id, method = Left GET, responseFormat = json }
  response <- request simpleListRequest
  case response of
    Left err -> pure $ Left (printError err)
    Right res -> do
      case decodeJson res.body of
        Right (r :: Array SimpleListEntry) -> pure $ Right r
        Left e -> do
          pure $ Left ("Can't parse JSON. " <> show e)

updateSimpleListEntry :: UpdateSimpleListEntry -> Aff (Either String String)
updateSimpleListEntry tle = do
  let
    body = encodeJson tle

    updateRequest = defaultRequest { url = getBaseUrl <> "/api/v1/entries", method = Left PUT, responseFormat = ignore, content = Just $ Json body }
  response <- request updateRequest
  case response of
    Left err -> pure $ Left (printError err)
    Right res -> pure $ Right ("Change me to unit")

deleteSimpleListEntry :: String -> Aff (Either String String)
deleteSimpleListEntry id = do
  let
    deleteRequest = defaultRequest { url = getBaseUrl <> "/api/v1/entries/" <> id, method = Left DELETE, responseFormat = ignore }
  response <- request deleteRequest
  case response of
    Left err -> pure $ Left (printError err)
    Right res -> pure $ Right ("Change me to unit")

createSimpleListEntry :: CreateSimpleListEntry -> Aff (Either String String)
createSimpleListEntry tle = do
  let
    body = encodeJson tle

    updateRequest = defaultRequest { url = getBaseUrl <> "/api/v1/entries", method = Left POST, responseFormat = ignore, content = Just $ Json body }
  response <- request updateRequest
  case response of
    Left err -> pure $ Left (printError err)
    Right res -> pure $ Right ("Change me to unit")

