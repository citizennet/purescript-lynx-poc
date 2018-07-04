module App.Router where

import Prelude

import App.Components.Builder as Builder
import App.Components.Home as Home
import App.Data.Input.Handler as IH
import App.Data.Input.Type as I
import App.Data.Relate.Handler as RH
import App.Data.Relate.Type as R
import App.Data.Validate.Handler as VH
import App.Data.Validate.Type as V
import Effect.Aff (Aff)
import Data.Either (Either(..))
import Data.Either.Nested (Either3)
import Data.Foldable (oneOf)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Lynx.Components.Form as Form
import Lynx.Data.Graph (FormId(..))
import Ocelot.Block.Layout as Layout
import Routing.Match (Match, end, int, lit, root)

----------
-- Routes

data Route
  = Index
  | Form FormId
  | Builder FormId

instance showRoute :: Show Route where
  show Index = "Index"
  show (Form id) = "Form: " <> show id
  show (Builder id) = "Builder: " <> show id

route :: Match Route
route =
  root *> oneOf
  [ Form <<< FormId <$> (lit "forms" *> int)
  , Builder <<< FormId <$> (lit "builder" *> int)
  , pure Index
  ] <* end


----------
-- Router

data Query a
  = Navigate Route a

type Input = Route
type State = Route

type ChildQuery = Coproduct3
  (Form.Query V.Validate I.AppInput R.Relate)
  Builder.Query
  Home.Query

type ChildSlot = Either3 Unit Unit Unit

component :: H.Component HH.HTML Query Input Void Aff
component =
  H.parentComponent
  { initialState: identity
  , render
  , eval
  , receiver: const Nothing
  }
  where
    render
      :: State
      -> H.ParentHTML Query ChildQuery ChildSlot Aff
    render = case _ of
      Index ->
        HH.slot' CP.cp3 unit Home.component unit (const Nothing)

      Form formId ->
        Layout.container
          [ HP.attr (H.AttrName "style") "max-width:40rem;" ]
          [ -- Look up formId and load that configuration with handleX functions
            HH.slot'
              CP.cp1
              unit
              ( Form.component
                { handleValidate: VH.handleValidate
                , handleInput: IH.handleInput
                , handleRelate: RH.handleRelate
                , initialize: Builder.initialize
                }
              )
              (Right formId)
              (const Nothing)
          ]

      Builder formId ->
        HH.slot' CP.cp2 unit Builder.component formId (const Nothing)

    eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void Aff
    eval = case _ of
      Navigate r a -> a <$ H.put r
