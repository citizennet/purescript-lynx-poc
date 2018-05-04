module App.Data.Input.Type where

import Prelude

import Data.Argonaut (jsonEmptyObject)
import Data.Argonaut.Decode (decodeJson, (.?))
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode ((:=), (~>))
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Either (Either(..))
import Data.String.Read (class Read, read)
import Data.Maybe (Maybe(..))

data Input
  = Text
  | TextArea
  | Number

instance showInput :: Show Input where
  show Text = "Text"
  show TextArea = "TextArea"
  show Number = "Number"

instance readInput :: Read Input where
  read "Text"     = Just Text
  read "TextArea" = Just TextArea
  read "Number"   = Just Number
  read _ = Nothing

instance encodeJsonSignupInput :: EncodeJson Input where
  encodeJson i = "inputType" := show i ~> jsonEmptyObject

instance decodeJsonSignupInput :: DecodeJson Input where
  decodeJson json = do
    obj <- decodeJson json
    str <- obj .? "inputType"
    input <- case read str of
      Just x -> pure x
      other -> Left $ "No case written to decode:\n" <> "  " <> str <> "\n"
    pure input
