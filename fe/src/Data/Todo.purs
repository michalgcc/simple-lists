module SimpleLists.Data.Todo
  ( Todo
  , TodoEntry
  ) where

type Todo = { entries :: Array TodoEntry }

type TodoEntry =
  { id :: Int
  , text :: String
  , isDone :: Boolean
  }
