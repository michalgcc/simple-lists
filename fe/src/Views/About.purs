module SimpleLists.Views.About
  ( aboutView
  ) where

import Prelude
import Effect.Aff.Class (class MonadAff)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap5 as B

aboutView :: forall q i o m. MonadAff m => H.Component q i o m
aboutView =
  H.mkComponent
    { initialState: \_ -> unit
    , render
    , eval: H.mkEval H.defaultEval
    }

render :: forall cs m. Unit -> H.ComponentHTML Unit cs m
render _ = HH.div [ HP.classes [ B.container ] ] [ HH.text "About" ]
