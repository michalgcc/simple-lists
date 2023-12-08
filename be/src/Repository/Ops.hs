{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

module Repository.Ops where

import           Control.Monad.Trans.Reader (ask)
import           Data.Int
import           Data.Text                  (Text)
import           Database.Beam
import           Database.Beam.Sqlite
import           Database.SQLite.Simple     (Connection, withConnection)
import           Domain                     (SimpleList (..),
                                             SimpleListEntry (..))
import           Repository.Data            (Entry, EntryT (..), List,
                                             ListT (..))
import           Repository.Db
import           Shared

mapE :: Entry -> SimpleListEntry
mapE e =
  SimpleListEntry {text = _entryText e, isDone = _entryIsDone e, id = _entryId e, listId = _entryListId e, updatedAt = _entryUpdatedAt e}

mapL :: List -> SimpleList
mapL l =
  SimpleList {id = _listId l, name = _listName l, updatedAt = _listUpdatedAt l}

mapSE :: SimpleListEntry -> Entry
mapSE e =
  Entry {_entryId = e.id, _entryIsDone = e.isDone, _entryText = e.text, _entryListId = e.listId, _entryUpdatedAt = e.updatedAt}

mapLE :: SimpleList -> List
mapLE l =
  List {_listId = l.id, _listUpdatedAt = l.updatedAt, _listName = l.name}

getAllSimpleLists :: CustomAppM [SimpleList]
getAllSimpleLists = do
  appState <- ask
  let all = orderBy_ (desc_ . _listUpdatedAt) $ all_ (lists simpleListDb)
  liftIO $ withConnection appState.db_conn_string $ \conn ->
    runBeamSqlite conn $ do
      result <- runSelectReturningList $ select all
      pure (map mapL result)

getSimpleList :: Int64 -> CustomAppM (Maybe SimpleList)
getSimpleList id = do
  appState <- ask
  liftIO $ withConnection appState.db_conn_string $ \conn ->
    runBeamSqlite conn $ do
      result <- runSelectReturningOne $ select (filter_ (\l -> _listId l ==. val_ id) $ all_ (lists simpleListDb))
      case result of
        Just list -> pure (Just (mapL list))
        _         -> pure Nothing

getSimpleListEntry :: Int64 -> CustomAppM (Maybe SimpleListEntry)
getSimpleListEntry id = do
  appState <- ask
  liftIO $ withConnection appState.db_conn_string $ \conn ->
    runBeamSqlite conn $ do
      result <- runSelectReturningOne $ select (filter_ (\le -> _entryId le ==. val_ id) $ all_ (entries simpleListDb))
      case result of
        Just entry -> pure $ Just $ mapE entry
        _          -> pure Nothing

getAllSimpleListEntriesByListId :: Int64 -> CustomAppM [SimpleListEntry]
getAllSimpleListEntriesByListId id = do
  appState <- ask
  let all = filter_ (\s -> _entryListId s ==. val_ id) $ all_ (entries simpleListDb)
  liftIO $ withConnection appState.db_conn_string $ \conn ->
    runBeamSqlite conn $ do
      result <- runSelectReturningList $ select all
      pure (map mapE result)

getAllSimpleListEntries :: CustomAppM [SimpleListEntry]
getAllSimpleListEntries = do
  appState <- ask
  let all = all_ (entries simpleListDb)
  liftIO $ withConnection appState.db_conn_string $ \conn ->
    runBeamSqlite conn $ do
      result <- runSelectReturningList $ select all
      pure (map mapE result)

insertSimpleListEntry :: SimpleListEntry -> CustomAppM ()
insertSimpleListEntry e = do
  appState <- ask
  let entry = mapSE e
  liftIO $ withConnection appState.db_conn_string $ \conn ->
    runBeamSqlite conn $ do
      runInsert $
        insert (entries simpleListDb) $
          insertValues [entry]

insertSimpleList :: SimpleList -> CustomAppM ()
insertSimpleList e = do
  appState <- ask
  let entry = mapLE e
  liftIO $ withConnection appState.db_conn_string $ \conn ->
    runBeamSqlite conn $ do
      runInsert $
        insert (lists simpleListDb) $
          insertValues [entry]

updateSimpleListEntry :: SimpleListEntry -> CustomAppM ()
updateSimpleListEntry e = do
  appState <- ask
  let entry = mapSE e
  liftIO $ withConnection appState.db_conn_string $ \conn ->
    runBeamSqlite conn $
      do
        runUpdate $
          save (entries simpleListDb) entry

updateSimpleList :: SimpleList -> CustomAppM ()
updateSimpleList e = do
  appState <- ask
  let entry = mapLE e
  liftIO $ withConnection appState.db_conn_string $ \conn ->
    runBeamSqlite conn $ do
      runUpdate $
        save (lists simpleListDb) entry

deleteSimpleListEntry :: Int64 -> CustomAppM ()
deleteSimpleListEntry id = do
  appState <- ask
  liftIO $ withConnection appState.db_conn_string $ \conn ->
    runBeamSqlite conn $
      do
        runDelete $
          delete (entries simpleListDb) (\e -> _entryId e ==. val_ id)

deleteSimpleList :: Int64 -> CustomAppM ()
deleteSimpleList id = do
  appState <- ask
  liftIO $ withConnection appState.db_conn_string $ \conn ->
    runBeamSqlite conn $
      do
        runDelete $
          delete (lists simpleListDb) (\e -> _listId e ==. val_ id)
