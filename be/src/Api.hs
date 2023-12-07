{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import           Data.Aeson
import           Data.Int
import           Data.Proxy
import qualified Data.Text    as T
import           GHC.Generics (Generic)
import           Servant.API

data SimpleList = SimpleList {entries :: [SimpleListEntry]}
  deriving (Show, Eq, Read, Generic)

instance FromJSON SimpleList

instance ToJSON SimpleList

data SimpleListEntry = SimpleListEntry {id :: Int32, text :: T.Text, isDone :: Bool}
  deriving (Show, Eq, Read, Generic)

instance FromJSON SimpleListEntry

instance ToJSON SimpleListEntry

type SimpleListAPI =
  "api"
    :> "v1"
    :> "entries"
    :> ( Get '[JSON] SimpleList
           :<|> ReqBody '[JSON] SimpleListEntry :> Post '[JSON] NoContent
           :<|> ReqBody '[JSON] SimpleListEntry :> Put '[JSON] NoContent
           :<|> Capture "id" Int32 :> Delete '[JSON] NoContent
       )
    :<|> Raw

simpleListApi :: Proxy SimpleListAPI
simpleListApi = Proxy
