module App.Data.Validate.Handler where

import App.Data.Validate.Type (Validate(..))
import Data.Either (Either(..))
import Data.String (null) as String

handleValidate :: Validate -> String -> Either String String
handleValidate v str = case v of
  Required ->
    if String.null str
      then Left "Field is required"
      else Right str
