module Lynx.Validation where

import Prelude

import Data.Array (any, elem)
import Data.String (Pattern(Pattern), contains, length, toCharArray)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, inj)
import Polyform.Validation (V(Invalid), Validation)
import Polyform.Validation as Validation
import Type.Prelude (SProxy(..))

malformed :: ∀ m err
  . Monad m
  => Validation m (Array (Variant (malformed :: String | err))) String String
malformed = Validation.hoistFnV \str →
  if contains (Pattern "@") str
    then pure str
    else Invalid [ inj (SProxy :: SProxy "malformed") str ]

tooShort :: ∀ m err
  . Monad m
 => Int
 -> String
 -> Validation m (Array (Variant (tooShort :: Tuple Int String | err))) String String
tooShort min text = Validation.hoistFnV \str ->
  if length str > min
    then pure str
    else Invalid [ inj (SProxy ∷ SProxy "tooShort") (Tuple min str) ]

-- Another version. In `Component` you can see how we can use this information to write
-- custom error messages.
tooLong :: ∀ m err
  . Monad m
 => Int
 -> Validation m (Array (Variant (tooLong :: Tuple Int String | err))) String String
tooLong max = Validation.hoistFnV \str →
  if length str < max
    then pure str
    else Invalid [ inj (SProxy :: SProxy "tooLong") (Tuple max str) ]

-- Another simple validator; this one ensures that the input contains a digit.
missingDigit :: ∀ m err
  . Monad m
 => Validation m (Array (Variant (missingDigit :: String | err))) String String
missingDigit = Validation.hoistFnV \str →
  let
    chars = toCharArray str
  in
    if any (_ `elem` chars) (toCharArray "0123456789")
      then pure str
      else Invalid [ inj (SProxy :: SProxy "missingDigit") str ]


