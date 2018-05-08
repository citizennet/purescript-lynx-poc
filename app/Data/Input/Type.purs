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
type AppInput = Input Attrs OptionItems
type Error = Array String

-- The input data type should attempt to hold only the constructor representing
-- the field type and some attributes necessary for rendering. Avoid any actual
-- data, as that's the form's responsibility once run. We should try to share as
-- many attributes among fields as is possible.
--
-- Not clear how to represent checkboxes with some variable options, for example.
-- We'll probably have to name them in some big constructor like this except for
-- option types.
data Input attrs items
  = Text       attrs (FormInput Error String String)
  | TextArea   attrs (FormInput Error String String)
  | Number     attrs (FormInput Error String Number)
  | Options    attrs (FormInput Error (InputOptions items) (InputOptions items))

instance encodeJsonInput
  :: (EncodeJson attrs, EncodeJson items)
  => EncodeJson (Input attrs items)
  where
  encodeJson i = case i of
    Text attrs input ->
      "inputType" := "Text"
      ~> "inputAttrs" := encodeJson attrs
      ~> "inputContents" := encodeJson input
      ~> jsonEmptyObject
    TextArea attrs input ->
      "inputType" := "TextArea"
      ~> "inputAttrs" := encodeJson attrs
      ~> "inputContents" := encodeJson input
      ~> jsonEmptyObject
    Number attrs input ->
      "inputType" := "Number"
      ~> "inputAttrs" := encodeJson attrs
      ~> "inputContents" := encodeJson input
      ~> jsonEmptyObject
    Options attrs input ->
      "inputType" := "Options"
      ~> "inputAttrs" := encodeJson attrs
      ~> "inputContents" := encodeJson input
      ~> jsonEmptyObject

instance decodeJsonInput
  :: (DecodeJson attrs, DecodeJson items)
  => DecodeJson (Input attrs items)
  where
  decodeJson json = do
    obj <- decodeJson json
    type' <- obj .? "inputType"
    attrs <- obj .? "inputAttrs"
    case type' of
      "Text" -> do
         contents <- obj .? "inputContents"
         pure $ Text attrs contents
      "TextArea" -> do
         contents <- obj .? "inputContents"
         pure $ TextArea attrs contents
      "Number" -> do
         contents <- obj .? "inputContents"
         pure $ Number attrs contents
      "Options" -> do
         contents <- obj .? "inputContents"
         pure $ Options attrs contents
      _ -> Left $ "No decoder written for case " <> type'

-- Inputs need to have raw values stored in the form so
-- the data can be pulled off and submitted.
newtype FormInput e i o = FormInput
  { input    :: i
  , result   :: Either e o
  , validate :: Boolean
  }

instance encodeJsonFormInput
  :: (EncodeJson e, EncodeJson i, EncodeJson o)
  => EncodeJson (FormInput e i o)
  where
    encodeJson (FormInput { input, result, validate }) =
      "input" := encodeJson input
      ~> "result" := encodeJson result
      ~> "validate" := encodeJson validate
      ~> jsonEmptyObject

instance decodeJsonFormInput
  :: (DecodeJson e, DecodeJson i, DecodeJson o)
  => DecodeJson (FormInput e i o)
  where
  decodeJson json = do
    obj <- decodeJson json
    input <- obj .? "input"
    result <- obj .? "result"
    validate <- obj .? "validate"
    pure $ FormInput { input, result, validate }

-- These can represent collections of options, where we once again need some top
-- level unified constructor for all possible 'items' we'll support.
data InputOptions items
  = Radio    (Array items)
  | Dropdown (Array items)
  | Checkbox (Array items)

instance encodeJsonInputOptions :: EncodeJson items => EncodeJson (InputOptions items) where
  encodeJson i =
    "optionType" := encodeJson type'
    ~> "optionItems" := encodeJson items
    ~> jsonEmptyObject
    where
      (Tuple type' items) = case i of
        Radio arr    -> Tuple "Radio" arr
        Dropdown arr -> Tuple "Dropdown" arr
        Checkbox arr -> Tuple "Checkbox" arr

instance decodeJsonInputOptions :: DecodeJson items => DecodeJson (InputOptions items) where
  decodeJson json = do
    obj <- decodeJson json
    type' <- obj .? "optionType"
    items <- obj .? "optionItems"
    case type' of
      "Radio" -> pure $ Radio items
      "Dropdown" -> pure $ Dropdown items
      "Checkbox" -> pure $ Checkbox items
      _ -> Left $ "No decoder written for case " <> type'

data OptionItems
  = TextItem String
  | CustomItem MyItem

instance encodeJsonOptionItems :: EncodeJson OptionItems where
  encodeJson i =
    "itemType" := encodeJson typ
    ~> "itemValue" := encodeJson val
    ~> jsonEmptyObject
    where
      (Tuple typ val) = case i of
        TextItem s -> Tuple "TextItem" s
        CustomItem (MyItem s) -> Tuple "MyItem" s

instance decodeJsonOptionItems :: DecodeJson OptionItems where
  decodeJson json = do
    obj <- decodeJson json
    type' <- obj .? "itemType"
    value <- obj .? "itemValue"
    case type' of
      "TextItem" -> pure $ TextItem value
      "CustomItem" -> pure $ CustomItem (MyItem value)
      _ -> Left $ "No decoder written for case " <> value

newtype MyItem = MyItem String

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
    ~> "helpText" := i.helpText
    ~> jsonEmptyObject

instance decodeJsonAttrs :: DecodeJson Attrs where
  decodeJson json = do
    obj <- decodeJson json
    label <- obj .? "label"
    helpText <- obj .?? "helpText" .?= Nothing
    pure $ Attrs { label, helpText }
