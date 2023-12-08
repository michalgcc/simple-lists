module Shared where

import           Control.Monad.Trans.Reader
import           Data.Int
import           Data.Snowflake             (SnowflakeGen, nextSnowflake,
                                             snowflakeToInteger)
import           Servant

data AppState = AppState
  { db_conn_string :: String,
    snowflake_gen  :: SnowflakeGen
  }

type CustomAppM = ReaderT AppState Handler

generateSnowflakeId :: AppState -> IO Int64
generateSnowflakeId as = do
  sId <- nextSnowflake as.snowflake_gen
  pure $ fromInteger $ snowflakeToInteger sId
