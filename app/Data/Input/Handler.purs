module App.Data.Input.Handler where

import Prelude

import App.Data.Input.Type (Input(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Lynx.Components.Form as Form
import Lynx.Data.Graph (InputRef)
import Ocelot.Block.Input as Input
import Ocelot.Block.FormField as FormField

handleInput :: ∀ v r
	 . Form.State v Input r
  -> InputRef
	-> H.ComponentHTML (Form.Query v Input r)
handleInput st ref = fromMaybe (HH.div_ []) $ getHtml <$> config
	where
		attr = HP.attr (HH.AttrName "data-inputref") (show $ unwrap ref)
		config = Map.lookup ref (_.inputs $ unwrap st.config)
		getHtml = renderInput <<< _.inputType <<< unwrap

		renderInput = case _ of
			Text -> FormField.field_
				{ helpText: Nothing
				, label: ""
				, error: Nothing
				, inputId: show <<< unwrap $ ref
				}
				[ Input.input
					[ attr, HP.value $ fromMaybe "" $ Map.lookup ref st.form ]
				]

			TextArea -> renderInput Text
			Number -> renderInput Text
