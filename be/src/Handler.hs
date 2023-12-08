{-# LANGUAGE LambdaCase #-}

module Handler where

import           Control.Exception
import           Data.Int
import qualified Data.Text         as T
import           Domain
import           Repository.Ops
import           Servant
import           Shared

-- TODO remove handler from names and use qualified imports

handlerGetAllSimpleLists :: CustomAppM [GetSimpleList]
handlerGetAllSimpleLists = getAllSimpleLists >>= mapM mapSimpleList

handlerGetAllSimpleListEntriesByListId :: Maybe Int64 -> CustomAppM [GetSimpleListEntry]
handlerGetAllSimpleListEntriesByListId =
  \case
    Just id -> getAllSimpleListEntriesByListId id >>= mapM mapSimpleListEntry
    Nothing -> getAllSimpleListEntries >>= mapM mapSimpleListEntry

-- curl -X POST -H "Content-Type: application/json" -d '{"name":"Some list"}' http://localhost:9292/api/v1/lists -v
handlerPostSimpleList :: PostSimpleList -> CustomAppM NoContent
handlerPostSimpleList e = do
  mapPostSimpleList e >>= insertSimpleList
  pure NoContent

handlerPutSimpleList :: PutSimpleList -> CustomAppM NoContent
handlerPutSimpleList e = do
  mapPutSimpleList e >>= updateSimpleList
  pure NoContent

handlerDeleteSimpleList :: T.Text -> CustomAppM NoContent
handlerDeleteSimpleList id = do
  mapId id >>= deleteSimpleList
  pure NoContent

-- curl -X POST -H "Content-Type: application/json" -d '{"id":11,"isDone":false,"text":"Buy groceries"}' http://localhost:9292/api/v1/entries -v
handlerPostSimpleListEntry :: PostSimpleListEntry -> CustomAppM NoContent
handlerPostSimpleListEntry e = do
  mapped <- mapPostSimpleListEntry e
  list <- getSimpleList mapped.listId
  case list of
    Nothing -> throw NotFoundException
    Just l -> do
      updatedList <- setSimpleListUpdatedAtToNow l
      updateSimpleList updatedList
      insertSimpleListEntry mapped
      pure NoContent

handlerPutSimpleListEntry :: PutSimpleListEntry -> CustomAppM NoContent
handlerPutSimpleListEntry e = do
  mapped <- mapPutSimpleListEntry e
  list <- getSimpleList mapped.listId
  case list of
    Nothing -> throw NotFoundException
    Just l -> do
      updatedList <- setSimpleListUpdatedAtToNow l
      updateSimpleList updatedList
      updateSimpleListEntry mapped
      pure NoContent

handlerDeleteSimpleListEntry :: T.Text -> CustomAppM NoContent
handlerDeleteSimpleListEntry id = do
  mappedId <- mapId id
  entry <- getSimpleListEntry mappedId
  case entry of
    Nothing -> throw NotFoundException
    Just e -> do
      list <- getSimpleList e.listId
      case list of
        Nothing -> throw NotFoundException
        Just l -> do
          updatedList <- setSimpleListUpdatedAtToNow l
          updateSimpleList updatedList
          deleteSimpleListEntry mappedId
          pure NoContent
