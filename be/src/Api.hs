{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import           Data.Aeson
import           Data.Int
import           Data.Proxy
import qualified Data.Text                      as T
import           Domain                         (GetSimpleList,
                                                 GetSimpleListEntry,
                                                 PostSimpleList,
                                                 PostSimpleListEntry,
                                                 PutSimpleList,
                                                 PutSimpleListEntry, SimpleList,
                                                 SimpleListEntry)
import           GHC.Generics                   (Generic)
import           Handler
import           Network.Wai.Application.Static
import           Servant
import           Servant.API
import           Shared
import           WaiAppStatic.Types

type SimpleListAPI =
  "api"
    :> "v1"
    :> ( ( "lists"
             :> ( Get '[JSON] [GetSimpleList]
                    :<|> ReqBody '[JSON] PostSimpleList :> Post '[JSON] NoContent
                    :<|> ReqBody '[JSON] PutSimpleList :> Put '[JSON] NoContent
                    :<|> Capture "id" T.Text :> Delete '[JSON] NoContent
                )
         )
           :<|> ( "entries"
                    :> ( QueryParam "listId" Int64 :> Get '[JSON] [GetSimpleListEntry]
                           :<|> ReqBody '[JSON] PostSimpleListEntry :> Post '[JSON] NoContent
                           :<|> ReqBody '[JSON] PutSimpleListEntry :> Put '[JSON] NoContent
                           :<|> Capture "id" T.Text :> Delete '[JSON] NoContent
                       )
                )
       )
    :<|> Raw

simpleListApi :: Proxy SimpleListAPI
simpleListApi = Proxy

server :: ServerT SimpleListAPI CustomAppM
server =
  ( ( handlerGetAllSimpleLists
        :<|> handlerPostSimpleList
        :<|> handlerPutSimpleList
        :<|> handlerDeleteSimpleList
    )
      :<|> ( handlerGetAllSimpleListEntriesByListId
               :<|> handlerPostSimpleListEntry
               :<|> handlerPutSimpleListEntry
               :<|> handlerDeleteSimpleListEntry
           )
  )
    :<|> serveDirectoryWith (returnIndexIfNotFoundSettings "static/")
  where
    returnIndexIfNotFoundSettings root = ds {ssLookupFile = lookup}
      where
        ds = defaultFileServerSettings root
        lookup p = do
          f <- ssLookupFile ds p
          case f of
            LRFile f   -> return $ LRFile f
            LRFolder f -> return $ LRFolder f
            LRNotFound -> ssLookupFile ds [unsafeToPiece "index.html"]
