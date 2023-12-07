module Shared where

import           Control.Monad.Trans.Reader
import           Servant

data AppState = AppState
  { db_conn_string :: String
  }

type CustomAppM = ReaderT AppState Handler
