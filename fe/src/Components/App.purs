module SimpleLists.Components.App where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Halogen.Router.Class (class MonadRouter)
import Halogen.Router.UseRouter (useRouter)
import SimpleLists.Components.Header (headerMenu)
import SimpleLists.Data.Route (AppRoute(..))
import SimpleLists.Views.About (aboutView)
import SimpleLists.Views.Home (homeView)
import Type.Proxy (Proxy(..))

app
  :: forall q i o m
   . MonadRouter AppRoute m
  => MonadAff m
  => H.Component q i o m
app =
  Hooks.component \_ _ -> Hooks.do
    current /\ _ <- useRouter
    Hooks.pure do
      HH.div [ HP.id "app" ]
        [ HH.slot_ (Proxy :: Proxy "headerMenu") unit headerMenu {}, HH.div_ [ routerView current ] ]
  where
  routerView :: Maybe AppRoute -> HH.HTML _ _
  routerView = case _ of
    Nothing -> HH.text "404"
    Just route -> case route of
      Home -> HH.slot_ (Proxy :: Proxy "home") unit homeView {}
      About -> HH.slot_ (Proxy :: Proxy "about") unit aboutView {}
