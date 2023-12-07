module Main where

import Prelude
import Effect (Effect)
import Effect.Class (liftEffect)
import Halogen (hoist)
import Halogen.Aff as HA
import Halogen.Router.Trans.PushState (mkRouter, runRouterT)
import Halogen.VDom.Driver (runUI)
import SimpleLists.Components.App (app)
import SimpleLists.Data.Route (routeCodec)

-- Replaced by cog before version bump
version :: String
version = "0.2.0"

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    router <- liftEffect $ mkRouter routeCodec
    let
      rootComponent = hoist (runRouterT router) app
    runUI rootComponent unit body
