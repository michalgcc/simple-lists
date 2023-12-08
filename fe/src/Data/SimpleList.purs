module SimpleLists.Data.SimpleList
  ( CreateSimpleList
  , CreateSimpleListEntry
  , SimpleList
  , SimpleListEntry
  , UpdateSimpleList
  , UpdateSimpleListEntry
  ) where

type SimpleList =
  { id :: String
  , name :: String
  , updatedAt :: String
  }

type CreateSimpleList = { name :: String }

type UpdateSimpleList =
  { id :: String
  , name :: String
  }

type SimpleListEntry =
  { id :: String
  , text :: String
  , isDone :: Boolean
  , listId :: String
  , updatedAt :: String
  }

type CreateSimpleListEntry =
  { text :: String
  , isDone :: Boolean
  , listId :: String
  }

type UpdateSimpleListEntry =
  { id :: String
  , text :: String
  , isDone :: Boolean
  , listId :: String
  }