module SimpleLists.Components.Header where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Halogen.Router.Class (class MonadRouter)
import Halogen.Router.UseRouter (useRouter)
import SimpleLists.Data.Route (AppRoute(..))

headerMenu
  :: forall q i o m
   . MonadRouter AppRoute m
  => H.Component q i o m
headerMenu =
  Hooks.component \_ _ -> Hooks.do
    current /\ { navigate } <- useRouter
    let
      routerLink :: AppRoute -> HH.HTML _ _
      routerLink route =
        let
          label = case _ of
            Home _ -> "Home"
            About -> "About"
            _ -> "Unknown"
        in
          HH.li []
            [ HH.a
                [ HE.onClick \_ -> navigate route
                , HP.href "javascript:"
                ]
                [ HH.text $ label route ]
            ]
    Hooks.pure do
      HH.header []
        [
        -- TODO refactor, for now it's not necessary
        -- HH.nav []
        --   [ HH.ul []
        --       (routerLink <$> [ Home Nothing, About ])
        --   ]
        ]
