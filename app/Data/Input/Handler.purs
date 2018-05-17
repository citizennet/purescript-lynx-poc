module App.Data.Input.Handler where

import Prelude

import App.Data.Input.Type (AppInput, Attrs(..), ForeignData(..), FormInput(..), Input(..), InputOptions(..), MyItem(..), OptionItems(..))
import Control.Monad.Aff.Class (class MonadAff)
import Data.Array (head)
import Data.Bifunctor (lmap)
import Data.Either (either)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Lynx.Components.Form as Form
import Lynx.Data.ForeignAPI (fetch)
import Lynx.Data.Graph (InputRef)
import Ocelot.Block.Checkbox (checkbox_) as Checkbox
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Input as Input
import Ocelot.Block.Radio as Radio
import Ocelot.Components.Typeahead as TA
import Ocelot.Components.Typeahead.Input as TAInput

-- Helper to stringify custom option items
optionItemToStr :: OptionItems -> String
optionItemToStr = case _ of
  TextItem s -> s
  CustomItem (MyItem s) -> s

-- Remember that this is the render function being passed in, so
-- we have to set the blur events we want to trigger.
handleInput :: ∀ v r eff m
 . MonadAff (Form.Effects eff) m
=> Form.State v AppInput r
-> InputRef
-> Form.ComponentHTML v AppInput r (Form.Effects eff) m
handleInput st ref = fromMaybe (HH.div_ [])
  $ renderInput <$> Map.lookup ref st.form
  where
    refStr = show <<< unwrap $ ref

    renderInput = case _ of
      Text (Attrs { helpText, label }) (FormInput { input, result, validate }) ->
        FormField.field_
        { helpText
        , label
        , error: either head (const Nothing) result
        , inputId: refStr
        }
        [ Input.input
          [ HP.attr (HH.AttrName "data-inputref") refStr
          , HP.value input
          , HE.onValueInput $ HE.input $ Form.UpdateValue ref <<< setTextValue
          , HE.onBlur $ HE.input_ $ Form.Blur ref resetV
          ]
        ]

      TextArea (Attrs { helpText, label }) (FormInput { input, result, validate }) ->
        FormField.field_
        { helpText
        , label
        , error: either head (const Nothing) result
        , inputId: refStr
        }
        [ Input.textarea
          [ HP.attr (HH.AttrName "data-inputref") refStr
          , HP.value input
          , HE.onValueInput $ HE.input $ Form.UpdateValue ref <<< setTextValue
          , HE.onBlur $ HE.input_ $ Form.Blur ref resetV
          ]
        ]

      Number (Attrs { helpText, label }) (FormInput { input, result }) ->
        FormField.field_
        { helpText
        , label
        , error: either head (const Nothing) result
        , inputId: refStr
        }
        [ Input.input
          [ HP.attr (HH.AttrName "data-inputref") refStr
          , HP.value input
          , HE.onValueInput $ HE.input $ Form.UpdateValue ref <<< setTextValue
          , HE.onBlur $ HE.input_ $ Form.Blur ref resetV
          ]
        ]

      Options
        attrs@(Attrs { helpText, label })
        (FormInput { input, result })
        ->
      let fieldset arr block = FormField.fieldset_
            { label
            , helpText
            , inputId: refStr
            , error: either head (const Nothing) result
            }
            [ HH.div_ $
                arr # mapWithIndex \i v -> block i v
            ]

       in case input of
        Radio arr -> fieldset arr $ \i v ->
          Radio.radio_
            [ HP.name refStr
            , HP.checked (if i == 0 then true else false)
            , HE.onChange $ HE.input_ $ Form.Blur ref resetV
            ]
            [ HH.text $ optionItemToStr v ]

        Checkbox arr -> fieldset arr $ \i v ->
          Checkbox.checkbox_
            [ HP.name refStr
            , HP.checked false
            , HE.onChange $ HE.input_ $ Form.Blur ref resetV
            ]
            [ HH.text $ optionItemToStr v ]

        Dropdown arr ->
          FormField.fieldset_
            { label
            , helpText
            , inputId: refStr
            , error: either head (const Nothing) result
            }
            [ HH.slot unit TA.component
              ( TAInput.defSingle
                [ HP.placeholder "Type to search..."
                , HP.id_ refStr
                ]
                arr
                TAInput.renderItemString
              )
              ( HE.input $ Form.HandleTypeahead ref )
            ]

      -- Data is set remotely. For radios and checkboxes, fetch at render time.
      -- For dropdowns, accept a function to search.
      OptionsForeign
        attrs@(Attrs { helpText, label })
        formInput@(FormInput { input, result })
        (ForeignData url akeys ikeys)
        ->
      let fieldset arr block = FormField.fieldset_
            { label
            , helpText
            , inputId: refStr
            , error: either head (const Nothing) result
            }
            [ HH.div_ $
                arr # mapWithIndex \i v -> block i v
            ]
       in case input of
          Radio arr -> fieldset arr $ \i v ->
            Radio.radio_
              [ HP.name refStr
              , HP.checked (if i == 0 then true else false)
              , HE.onChange $ HE.input_ $ Form.Blur ref resetV
              ]
              [ HH.text v ]

          Checkbox arr -> fieldset arr $ \i v ->
            Checkbox.checkbox_
              [ HP.name refStr
              , HP.checked false
              , HE.onChange $ HE.input_ $ Form.Blur ref resetV
              ]
              [ HH.text v ]

          Dropdown _ ->
            FormField.fieldset_
              { label
              , helpText
              , inputId: refStr
              , error: either head (const Nothing) result
              }
              [ HH.slot unit TA.component
                ( TAInput.defAsyncMulti
                  [ HP.placeholder "Type to search..."
                  , HP.id_ refStr
                  ]
                  (fetch akeys ikeys url)
                  TAInput.renderItemString
                )
                ( HE.input $ Form.HandleTypeahead ref )
              ]

setTextValue :: String -> AppInput -> AppInput
setTextValue str inputType = case inputType of
  Text attrs (FormInput f@{ input }) ->
    Text attrs $ FormInput f { input = str }
  TextArea attrs (FormInput f@{ input }) ->
    TextArea attrs $ FormInput f { input = str }
  Number attrs (FormInput f@{ input }) ->
    Number attrs $ FormInput f { input = str }
  other -> inputType

resetV :: AppInput -> AppInput
resetV inputType = case inputType of
  Text attrs (FormInput f@{ result }) ->
    Text attrs $ FormInput f { result = lmap (const []) result }
  TextArea attrs (FormInput f@{ result }) ->
    TextArea attrs $ FormInput f { result = lmap (const []) result }
  Number attrs (FormInput f@{ result }) ->
    Number attrs $ FormInput f { result = lmap (const []) result }
  other -> inputType
