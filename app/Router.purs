module App.Router where

import Prelude

import App.Forms.Signup as Signup
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE)
import DOM (DOM)
import Data.Either (Either(..))
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Lynx.Components.Form as Form
import Lynx.Data.Graph (FormId(..))
import Network.HTTP.Affjax (AJAX)
import Routing.Match (Match)
import Routing.Match.Class (end, int, lit, root)

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

type ChildQuery
  = Form.Query Signup.SignupValidate Signup.SignupInput Signup.SignupRelation

type ChildSlot
  = Unit

type Effects eff = ( dom :: DOM, ajax :: AJAX, console :: CONSOLE | eff )

component :: ∀ e. H.Component HH.HTML Query Input Void (Aff (Effects e))
component =
  H.parentComponent
  { initialState: id
  , render
  , eval
  , receiver: const Nothing
  }
  where
    render :: State -> H.ParentHTML Query ChildQuery ChildSlot (Aff (Effects e))
    render = case _ of
      Index ->
        HH.div_ [ HH.text "Try /#/forms/0 or /#/builder/0" ]

      Form formId ->
        -- Look up formId and load that configuration with handleX functions
        HH.slot
          unit
          ( Form.component
            { handleValidate: Signup.handleValidation
            , handleInput: Signup.renderInput
            , handleRelate: Signup.handleRelation
            }
          )
          (Right formId)
          (const Nothing)

      Builder formId -> render $ Form formId

    eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void (Aff (Effects e))
    eval = case _ of
      Navigate r a -> a <$ H.put r
