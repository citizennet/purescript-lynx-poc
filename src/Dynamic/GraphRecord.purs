module Lynx.Dynamic.GraphRecord where

import Data.Variant
import Prelude

import Control.Monad.State (class MonadState, State, evalState, get, modify)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonEmptyObject, (.?), (:=), (~>))
import Data.Array ((:))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (class Newtype, wrap)
import Data.Record as Record
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Debug.Trace (spy)

----------
-- Form

-- The State transformer over FormConfig, allows us to use a monadic DSL based
-- on State to successively transform a configuration record.
type FormM f = State f

-- r: the possible set of relations for the form
type InputConfig r e a =
  { value :: Either e a
  , relations :: Array r
  }

runFormBuilder :: ∀ f a. f -> FormM f a -> a
runFormBuilder form = flip evalState form

data Action input
  = Fetch Boolean input
  | ResetDependents
  | Clear

email' =
  { value: Right "email@email.com"
  , actions: case _ of
      Clear -> pure unit
      Fetch bool (Tuple (SProxy :: SProxy "email") (SProxy :: SProxy "pass")) -> pure unit
      ResetDependents -> pure unit
  }

pass' =
  { value: Right "password"
  , actions: case _ of
      Clear -> pure unit
      Fetch bool (SProxy :: SProxy "email") -> pure unit
      ResetDependents -> pure unit
  }

email =
  { value: Right "email@email.com"
  , relations: []
  }

password :: forall t. IsSymbol t => InputConfig (SProxy t) String String
password = { value: Right "password", relations: [] }

type SignupForm r =
  { email :: InputConfig r String String
  , password :: InputConfig r String String
  }

form :: forall t. IsSymbol t => SignupForm (SProxy t)
form = { email: email, password: password }

--  -- Augment an input with a new relationship. Returns the original ref.
--  relate :: ∀ f r a. IsSymbol a => r -> SProxy a -> FormM (FormConfig r) (SProxy a)
relate relation name = do
  modify $ \form ->
     let field = Record.get name form
      in Record.set name (field { relations = relation : field.relations }) form
  pure name
