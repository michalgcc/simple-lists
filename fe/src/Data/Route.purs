module SimpleLists.Data.Route where

import Prelude
import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', path, root, segment, string)
import Routing.Duplex.Generic as G

data AppRoute
  = Home
  | About

derive instance Eq AppRoute
derive instance Generic AppRoute _

routeCodec :: RouteDuplex' AppRoute
routeCodec = root $ G.sum
  { "Home": G.noArgs
  , "About": path "about" G.noArgs
  }

