module App.Data.Input.Handler where

import Prelude

import App.Data.Input.Type (AppInput, Attrs(..), Input(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Lynx.Components.Form as Form
import Lynx.Data.Graph (InputRef)
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Input as Input

handleInput :: ∀ v r
 . Form.State v AppInput r
-> InputRef
-> H.ComponentHTML (Form.Query v AppInput r)
handleInput st ref = fromMaybe (HH.div_ [])
  $ renderInput <<< _.inputType <<< unwrap
  <$> Map.lookup ref (_.inputs $ unwrap st.config)
  where
    renderInput = case _ of
      Text (Attrs { helpText, label }) ->
        FormField.field_
        { helpText
        , label
        , error: Nothing
        , inputId: show <<< unwrap $ ref
        }
        [ Input.input
          [ HP.attr (HH.AttrName "data-inputref") (show $ unwrap ref)
          , HP.value $ fromMaybe "" $ Map.lookup ref st.form
          ]
        ]

      TextArea attrs -> renderInput $ Text attrs
      Number attrs -> renderInput $ Text attrs
