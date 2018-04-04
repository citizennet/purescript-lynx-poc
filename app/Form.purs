module App.Form where

import Prelude

import Control.Monad.State (gets)
import Data.Tuple (Tuple)
import Data.Variant (Variant)
import Lynx.Graph (Form, FormInput(..), clear, input, mustEqual, runFormBuilder)
import Lynx.Validation (missingDigit, tooLong, tooShort)
import Polyform.Validation (Validation)

-- | Used here, constructs the dsl into a Form
type User =
  { username :: String
  , password :: String
  }

userSignup :: Form
userSignup = runFormBuilder do
  user  <- input Text "Username"
  pass1 <- input Password "Password 1"
  pass2 <- input Password "Password 2"
    >>= (_ `clear` user)
    >>= (_ `mustEqual` pass1)
  gets _.inputs >>= \m -> pure { fields: m }


----------
-- Validation for the password field

type PasswordError = Variant
  ( missingDigit :: String
  , tooShort :: Tuple Int String
  , tooLong :: Tuple Int String
  )

passwordFieldValidation :: ∀ m
  . Monad m
 => Int
 -> Int
 -> Validation m (Array PasswordError) String String
passwordFieldValidation min max =
  tooShort min ( "Passwords must be at least "
               <> show min
               <> " characters long." )
  >>> tooLong max
  >>> missingDigit
