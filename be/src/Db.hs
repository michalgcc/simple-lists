{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

module Db where

import           Api                        (SimpleListEntry (..))
import           Control.Monad.Trans.Reader (ask)
import           Data.Int                   (Int32)
import           Data.Text                  (Text)
import           Database.Beam
import           Database.Beam.Sqlite
import           Database.SQLite.Simple     (Connection, withConnection)
import           Shared

type Entry = EntryT Identity

deriving instance Show Entry

deriving instance Eq Entry

type EntryId = PrimaryKey EntryT Identity

-- id :: Int, text :: T.Text, isDone :: Bool
data EntryT f = Entry
  { _entryId     :: Columnar f Int32,
    _entryText   :: Columnar f Text,
    _entryIsDone :: Columnar f Bool
  }
  deriving (Generic)

instance Beamable EntryT

instance Table EntryT where
  data PrimaryKey EntryT f = EntryId (Columnar f Int32) deriving (Generic, Beamable)
  primaryKey = EntryId . _entryId

newtype SimpleListDb f = SimpleListDb
  { entries :: f (TableEntity EntryT)
  }
  deriving (Generic, Database be)

simpleListDb :: DatabaseSettings be SimpleListDb
simpleListDb = defaultDbSettings

mapE :: Entry -> SimpleListEntry
mapE e =
  SimpleListEntry {text = _entryText e, isDone = _entryIsDone e, id = _entryId e}

mapSE :: SimpleListEntry -> Entry
mapSE e =
  Entry {_entryId = e.id, _entryIsDone = e.isDone, _entryText = e.text}

getAll :: CustomAppM [SimpleListEntry]
getAll = do
  x <- ask
  let all = all_ (entries simpleListDb)
  liftIO $ withConnection x.db_conn_string $ \conn ->
    runBeamSqlite conn $ do
      result <- runSelectReturningList $ select all
      return (map mapE result)

insertSLE :: SimpleListEntry -> CustomAppM ()
insertSLE e = do
  x <- ask
  let entry = mapSE e
  liftIO $ withConnection x.db_conn_string $ \conn ->
    runBeamSqlite conn $ do
      runInsert $
        insert (entries simpleListDb) $
          insertExpressions
            [ Entry default_ (val_ (_entryText entry)) (val_ (_entryIsDone entry))
            ]

updateSLE :: SimpleListEntry -> CustomAppM ()
updateSLE e = do
  x <- ask
  let entry = mapSE e
  liftIO $ withConnection x.db_conn_string $ \conn ->
    runBeamSqlite conn $
      do
        runUpdate $
          save (entries simpleListDb) entry

deleteSLE :: Int32 -> CustomAppM ()
deleteSLE id = do
  x <- ask
  liftIO $ withConnection x.db_conn_string $ \conn ->
    runBeamSqlite conn $
      do
        runDelete $
          delete (entries simpleListDb) (\e -> _entryId e ==. val_ id)
