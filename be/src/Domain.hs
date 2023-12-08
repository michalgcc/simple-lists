{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

module Domain where

import           Control.Exception          (throw)
import           Control.Exception.Base     (Exception)
import           Control.Monad.Trans.Reader (ask)
import           Data.Aeson
import           Data.Int
import qualified Data.Text                  as T
import           Data.Time                  (UTCTime, getCurrentTime)
import           Database.Beam
import           Shared                     (CustomAppM, generateSnowflakeId)
import           Text.Read                  (readMaybe)

data ReadIdException = ReadIdException
  deriving stock (Show)
  deriving anyclass (Exception)

data NotFoundException = NotFoundException
  deriving stock (Show)
  deriving anyclass (Exception)

data SimpleList = SimpleList
  { id        :: Int64,
    name      :: T.Text,
    updatedAt :: UTCTime
  }

data SimpleListEntry = SimpleListEntry
  { id        :: Int64,
    text      :: T.Text,
    isDone    :: Bool,
    listId    :: Int64,
    updatedAt :: UTCTime
  }

data GetSimpleList = GetSimpleList
  { id        :: T.Text,
    name      :: T.Text,
    updatedAt :: UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

newtype PostSimpleList = PostSimpleList
  { name :: T.Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data PutSimpleList = PutSimpleList
  { id   :: T.Text,
    name :: T.Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data GetSimpleListEntry = GetSimpleListEntry
  { id        :: T.Text,
    text      :: T.Text,
    isDone    :: Bool,
    listId    :: T.Text,
    updatedAt :: UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data PostSimpleListEntry = PostSimpleListEntry
  { text   :: T.Text,
    isDone :: Bool,
    listId :: T.Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data PutSimpleListEntry = PutSimpleListEntry
  { id     :: T.Text,
    text   :: T.Text,
    isDone :: Bool,
    listId :: T.Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

mapSimpleList :: SimpleList -> CustomAppM GetSimpleList
mapSimpleList l = do
  let sId = T.pack $ show l.id
  pure GetSimpleList {id = sId, name = l.name, updatedAt = l.updatedAt}

mapSimpleListEntry :: SimpleListEntry -> CustomAppM GetSimpleListEntry
mapSimpleListEntry l = do
  let sId = T.pack $ show l.id
  let sListId = T.pack $ show l.listId
  pure GetSimpleListEntry {id = sId, text = l.text, isDone = l.isDone, listId = sListId, updatedAt = l.updatedAt}

mapPostSimpleList :: PostSimpleList -> CustomAppM SimpleList
mapPostSimpleList l = do
  appState <- ask
  now <- liftIO getCurrentTime
  id <- liftIO $ generateSnowflakeId appState
  pure SimpleList {id = id, name = l.name, updatedAt = now}

mapPutSimpleList :: PutSimpleList -> CustomAppM SimpleList
mapPutSimpleList l = do
  let sId = readMaybe $ T.unpack l.id :: Maybe Int64
  case sId of
    Just id -> do
      now <- liftIO getCurrentTime
      pure SimpleList {id = id, name = l.name, updatedAt = now}
    _ -> throw ReadIdException

mapPostSimpleListEntry :: PostSimpleListEntry -> CustomAppM SimpleListEntry
mapPostSimpleListEntry l = do
  let lId = readMaybe $ T.unpack l.listId :: Maybe Int64
  case lId of
    Just listId -> do
      appState <- ask
      now <- liftIO getCurrentTime
      id <- liftIO $ generateSnowflakeId appState
      pure SimpleListEntry {id = id, text = l.text, isDone = l.isDone, listId = listId, updatedAt = now}
    _ -> throw ReadIdException

mapPutSimpleListEntry :: PutSimpleListEntry -> CustomAppM SimpleListEntry
mapPutSimpleListEntry l = do
  let sId = readMaybe $ T.unpack l.id :: Maybe Int64
  let lId = readMaybe $ T.unpack l.listId :: Maybe Int64

  case (sId, lId) of
    (Just sId, Just lId) -> do
      now <- liftIO getCurrentTime
      pure SimpleListEntry {id = sId, text = l.text, isDone = l.isDone, listId = lId, updatedAt = now}
    _ -> throw ReadIdException

mapId :: T.Text -> CustomAppM Int64
mapId id = do
  let mId = readMaybe $ T.unpack id :: Maybe Int64
  case mId of
    Just id -> pure id
    _       -> throw ReadIdException

setSimpleListUpdatedAtToNow :: SimpleList -> CustomAppM SimpleList
setSimpleListUpdatedAtToNow l = do
  now <- liftIO getCurrentTime
  pure SimpleList {id = l.id, name = l.name, updatedAt = now}
