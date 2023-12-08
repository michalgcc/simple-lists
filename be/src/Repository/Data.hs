{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module Repository.Data where

import           Data.Int
import qualified Data.Text     as T
import           Data.Time
import           Database.Beam

type Entry = EntryT Identity

type EntryId = PrimaryKey EntryT Identity

data EntryT f = Entry
  { _entryId        :: Columnar f Int64,
    _entryText      :: Columnar f T.Text,
    _entryIsDone    :: Columnar f Bool,
    _entryListId    :: Columnar f Int64,
    _entryUpdatedAt :: Columnar f UTCTime
  }
  deriving (Generic, Beamable)

instance Table EntryT where
  data PrimaryKey EntryT f = EntryId (Columnar f Int64) deriving (Generic, Beamable)
  primaryKey = EntryId . _entryId

type List = ListT Identity

type ListId = PrimaryKey ListT Identity

data ListT f = List
  { _listId        :: Columnar f Int64,
    _listName      :: Columnar f T.Text,
    _listUpdatedAt :: Columnar f UTCTime
  }
  deriving (Generic, Beamable)

instance Table ListT where
  data PrimaryKey ListT f = ListId (Columnar f Int64) deriving (Generic, Beamable)
  primaryKey = ListId . _listId
