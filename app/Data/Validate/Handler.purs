module App.Data.Validate.Handler where

import Prelude

import App.Data.Input.Type (AppInput, FormInput(..), Input(..))
import App.Data.Validate.Type (Validate(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.String as String

catEither :: ∀ a b
  . Semigroup a
 => Either a b
 -> Either a b
 -> Either a b
catEither (Left a) (Left b) = Left $ a <> b
catEither (Left a) (Right b) = Left a
catEither (Right a) (Left b) = Left b
catEither (Right a) (Right b) = Right a

handleValidate :: Validate -> AppInput -> AppInput
handleValidate v i = case i of
  Text attrs (FormInput f) ->
    Text
      attrs
      (FormInput $ f { result = catEither f.result (validateString v f.input) })
  TextArea attrs (FormInput f) ->
    TextArea
      attrs
      (FormInput $ f { result = catEither f.result (validateString v f.input) })
  Number attrs (FormInput f) ->
    Number
      attrs
      (FormInput $ f { result = catEither f.result (validateNumber v f.input) })
  _ -> i

validateString :: Validate -> String -> Either (Array String) String
validateString v str = case v of
  Required -> case String.null str of
    true -> Left [ "Field is required" ]
    _ -> pure str

  IsNumber -> case Number.fromString str of
    Nothing -> case String.null str of
      false -> Left [ "Must be a number." ]
      _ -> pure str
    _ -> pure str

validateNumber :: Validate -> String -> Either (Array String) Number
validateNumber v str = case v of
  Required -> case String.null str of
    true -> Left [ "Field is required" ]
    _ -> validateNumber IsNumber str

  IsNumber -> case Number.fromString str of
    Nothing -> case String.null str of
      false -> Left [ "Must be a number." ]
      _ -> pure 0.0
    (Just n) -> pure n
