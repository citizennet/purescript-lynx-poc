module App.Form where

import Prelude

import Control.Monad.State (gets)
import Data.Either (Either(..))
import Data.String as String
import Lynx.Graph (Form, FormInput(..), clear, input, mustEqual, runFormBuilder, validate)

-- | Used here, constructs the dsl into a Form
type User =
  { username :: String
  , password :: String
  }

userSignup :: Form UserValidate
userSignup = runFormBuilder do
  user  <- input Text "Username"
    >>= (_ `validate` NonEmpty)
  pass1 <- input Text "Password 1"
    >>= (_ `validate` InRange 5 15)
    >>= (_ `clear` user)
  pass2 <- input Text "Password 2"
    >>= (_ `validate` InRange 5 15)
    >>= (_ `mustEqual` pass1)
  gets _.inputs >>= \m -> pure { fields: m }

data UserValidate
  = InRange Int Int
  | NonEmpty

userValidation :: UserValidate -> String -> Either String String
userValidation v str = case v of
  NonEmpty ->
    if String.null str
      then Left "Field cannot be empty"
      else Right str
  InRange i0 i1 ->
    if String.length str < i0 || String.length str > i1
      then Left $ "Field must be between " <> show i0 <> " and " <> show i1 <> " characters."
      else Right str
