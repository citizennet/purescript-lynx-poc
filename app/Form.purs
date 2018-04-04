module App.Form where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console as Console
import Control.Monad.State (gets)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String as String
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events (input, input_, onBlur, onValueInput) as HE
import Halogen.HTML.Properties as HP
import Lynx.Component as Component
import Lynx.Graph (Form, InputRef, FormM, input, relate, runFormBuilder, validate)

type UserForm  = Form  UserValidate UserInput UserRelation User
type UserFormM = FormM UserValidate UserInput UserRelation User

type User =
  { username :: String
  , password :: String
  }

data UserInput
  = Text { label :: String }

data UserRelation
  = MustEqual InputRef
  | Clear InputRef

data UserValidate
  = InRange Int Int
  | NonEmpty


---------
-- FORM

-- A user signup form
userSignup :: UserForm
userSignup = runFormBuilder do
  user  <- input (Text { label: "Username" })
    >>= validate NonEmpty
  pass1 <- input (Text { label: "Password 1" })
    >>= validate (InRange 5 15)
    >>= relate (MustEqual user)
  pass2 <- input (Text { label: "Password 2" })
    >>= validate (InRange 5 15)
    >>= relate (Clear pass1)
  gets _.inputs >>= \m -> pure { fields: m }

-- A function to run user validation
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

-- A function to run user relations
userRelation :: ∀ eff
   . UserRelation
  -> InputRef
  -> H.ComponentDSL (Component.State UserValidate UserInput UserRelation User) Component.Query Component.Message (Aff (Component.Effects eff)) Unit
userRelation relation refA = case relation of
  MustEqual refB -> do
    st <- H.get
    let equal = do
          v0 <- Map.lookup refA st.form
          v1 <- Map.lookup refB st.form
          pure $ v0 == v1
    case equal of
      Just true -> pure unit
      otherwise -> do
        H.liftAff $ Console.log $ show refA <> " is NOT equal to " <> show refB
        pure unit

  Clear refB -> do
    H.modify \st -> st { form = Map.insert refB "" st.form }
    H.liftAff $ Console.logShow $ "Deleted " <> show refB
    pure unit

-- A function to render user inputs
renderUserInput
  :: Component.State UserValidate UserInput UserRelation User
  -> InputRef
  -> H.ComponentHTML Component.Query
renderUserInput st ref =
  let attr = HP.attr (HH.AttrName "data-inputref") (show $ unwrap ref)
      config = Map.lookup ref st.config.fields
   in case config of
        Just { inputType } -> case inputType of
          Text { label } ->
            HH.div_
              [ HH.text label
              , HH.input
                  [ attr
                  , HE.onValueInput $ HE.input $ Component.UpdateValue ref
                  , HE.onBlur $ HE.input_ $ Component.Blur ref
                  , HP.value $ fromMaybe "field not found in form! nooo" $ Map.lookup ref st.form
                  ]
              ]
        otherwise -> HH.div_ []
