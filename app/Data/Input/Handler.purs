module App.Data.Input.Handler where

import Prelude

import App.Data.Input.Type (AppInput, Attrs(..), Input(..), InputOptions(..), MyItem(..), OptionItems(..))
import Data.FunctorWithIndex (mapWithIndex)
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
import Ocelot.Block.Radio (radio_) as Radio


optionItemToStr :: OptionItems -> String
optionItemToStr = case _ of
  TextItem s -> s
  CustomItem (MyItem s) -> s

handleInput :: ∀ v r
 . Form.State v AppInput r
-> InputRef
-> H.ComponentHTML (Form.Query v AppInput r)
handleInput st ref = fromMaybe (HH.div_ [])
  $ renderInput <<< _.inputType <<< unwrap
  <$> Map.lookup ref (_.inputs $ unwrap st.config)
  where
    refStr = show <<< unwrap $ ref

    renderInput = case _ of
      Text (Attrs { helpText, label }) ->
        FormField.field_
        { helpText
        , label
        , error: Nothing
        , inputId: refStr
        }
        [ Input.input
          [ HP.attr (HH.AttrName "data-inputref") refStr
          , HP.value $ fromMaybe "" $ Map.lookup ref st.form
          ]
        ]

      TextArea attrs -> renderInput $ Text attrs
      Number attrs -> renderInput $ Text attrs

      Options attrs@(Attrs { helpText, label }) inputOpts -> case inputOpts of
        Radio arr ->
          FormField.fieldset_
          { label
          , helpText
          , inputId: refStr
          , error: Nothing
          }
          [ HH.div_ $
              arr # mapWithIndex \i v ->
                Radio.radio_
                  [ HP.name refStr, HP.checked (if i == 0 then true else false) ]
                  [ HH.text $ optionItemToStr v ]
          ]

        Dropdown arr -> renderInput $ Options attrs (Radio arr)
        Checkbox arr -> renderInput $ Options attrs (Radio arr)
