module App.Data.Validate.Handler where

import Prelude

import App.Data.Validate.Type (Validate(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.String as String

handleValidate :: Validate -> String -> Either String String
handleValidate v str = case v of
  Required -> case String.null str of
    true -> Left "Field is required"
    _ -> pure str

  IsNumber -> case Number.fromString str of
    Nothing -> Left "Must be a number."
    _ -> pure str
