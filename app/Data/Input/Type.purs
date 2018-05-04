module App.Data.Input.Type where

import Prelude

import Data.Argonaut (jsonEmptyObject)
import Data.Argonaut.Decode (decodeJson, (.?), (.??), (.?=))
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode (encodeJson, (:=), (~>))
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

-- For convenience.
type AppInput = Input Attrs

-- The input data type should attempt to hold only the constructor representing
-- the field type and some attributes necessary for rendering. Avoid any actual
-- data, as that's the form's responsibility once run. We should try to share as
-- many attributes among fields as is possible.
--
-- Not clear how to represent checkboxes with some variable options, for example.
-- We'll probably have to name them in some big constructor like this except for
-- option types.
data Input attrs
  = Text     attrs
  | TextArea attrs
  | Number   attrs

instance encodeJsonInput :: EncodeJson attrs => EncodeJson (Input attrs) where
  encodeJson i =
    "inputType" := itype
    ~> "inputAttrs" := encodeJson attrs
    ~> jsonEmptyObject
    where
      (Tuple itype attrs) = case i of
        Text x     -> Tuple "Text" x
        TextArea x -> Tuple "TextArea" x
        Number x   -> Tuple "Number" x

instance decodeJsonInput :: DecodeJson attrs => DecodeJson (Input attrs) where
  decodeJson json = do
    obj <- decodeJson json
    attrs <- obj .? "inputAttrs"
    str <- obj .? "inputType"
    case str of
      "Text" -> pure $ Text attrs
      "TextArea" -> pure $ TextArea attrs
      "Number" -> pure $ Number attrs
      _ -> Left $ "No decoder written for case " <> str

-- Attributes should only contain information about the display
-- of the input and nothing about the contents. No DOM input, no
-- data -- just information about rendering.
newtype Attrs = Attrs
  { label :: String
  , helpText :: Maybe String
  }

instance encodeJsonAttrs :: EncodeJson Attrs where
  encodeJson (Attrs i) =
    "label" := i.label
    ~> "helpText" := i.label
    ~> jsonEmptyObject

instance decodeJsonAttrs :: DecodeJson Attrs where
  decodeJson json = do
    obj <- decodeJson json
    label <- obj .? "label"
    helpText <- obj .?? "helpText" .?= Nothing
    pure $ Attrs { label, helpText }
