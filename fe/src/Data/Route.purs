module SimpleLists.Data.Route where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Routing.Duplex (RouteDuplex', optional, path, root, segment, string)
import Routing.Duplex.Generic as G

data AppRoute
  = Home (Maybe String)
  | About

derive instance Eq AppRoute
derive instance Generic AppRoute _

routeCodec :: RouteDuplex' AppRoute
routeCodec = root $ G.sum
  { "Home": (optional segment)
  , "About": path "about" G.noArgs
  }

