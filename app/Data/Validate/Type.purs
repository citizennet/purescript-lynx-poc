module App.Data.Validate.Type where

import Prelude

import Data.Argonaut (jsonEmptyObject)
import Data.Argonaut.Decode (decodeJson, (.?))
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode ((:=), (~>))
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.String.Read (class Read, read)

data Validate
  = Required
  | IsNumber

derive instance genericValidate :: Generic Validate _
instance eqValidate :: Eq Validate where
  eq = genericEq

instance showValidate :: Show Validate where
  show = genericShow

instance readValidate :: Read Validate where
  read "Required" = pure Required
  read "IsNumber" = pure IsNumber
  read _          = Nothing

instance encodeJsonValidate :: EncodeJson Validate where
  encodeJson i = "validationType" := show i ~> jsonEmptyObject

instance decodeJsonValidate :: DecodeJson Validate where
  decodeJson json = do
    obj <- decodeJson json
    str <- obj .? "validationType"
    relation <- case read str of
      Just x -> pure x
      other -> Left $ "No case written to decode:\n" <> "  " <> str <> "\n"
    pure relation
